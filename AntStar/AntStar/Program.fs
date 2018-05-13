// Learn more about F# at http://fsharp.org
module Program
open System
open System.IO
open Domain
open Grid
open Search
open System.Collections.Generic

let convertAction = function
    | NOP            -> "NoOp" |> Some 
    | Move (_,d)     -> "Move("+string d+")" |> Some 
    | Pull(_,ad,bd)  -> "Pull("+string ad+","+string bd+")" |> Some 
    | Push(_,ad,bd)  -> "Push("+string ad+","+string bd+")" |> Some 
    | MovePointer(_) -> None

let convertActionArray actionArray =
    let elems = Array.map convertAction actionArray
                |> Array.choose id
                |> String.concat ","
    let s = String.concat "" ["["; elems; "]"]
    eprintfn "%s" s
    s

let rec toOutput (l: (Action []) list): string list = List.map convertActionArray l
    
let rec printOutput (l:string list) = 
    match l with
    | []   -> []
    | h::t -> printfn "%s" h :: printOutput t

let printPossibleOutcomes state = Grid.allValidActions state 
                                  |> List.map (fun (_, s') -> printfn "%O" s') 
                                  |> ignore

let rec testActions state = function
    | action :: actions ->
        match apply action state with
        | Error err      -> failwith (err.ToString ())
        | Success state' -> printfn "%O" state'
                            testActions state' actions
    | [] -> printfn "Is Goal state: %O" (allGoalsMet state)

let getGridFromLines lines = 
  let colors, gridLines = parseColors Map.empty (lines)
  parseMap colors (gridLines |> addIdx)

let getGrid filename =
  let lines = Path.Combine(__SOURCE_DIRECTORY__, filename) |> readLines
  getGridFromLines lines

let getGoals grid =
  grid.staticGrid
  |> Map.map (fun _ v ->
    match v with
    | Goal c -> Some c
    | SEmpty -> None)
  |> Map.filter (fun _ x -> Option.isSome x)
  |> Map.map (fun _ x -> Option.get x)
  |> Map.toList

let unWrap = function
    | Success s -> s
    | Error _ -> failwith "nooooo"

// goal ordering (wait)
// solution path
// filter obstacles
// remove from path by recursive search

let searchAllPositions grid startPos =
  let fs,fq: Set<Pos> * Queue<Pos * int> = set [startPos], new Queue<Pos * int>()
  fq.Enqueue((startPos,0))
  let e: (Pos * int) list = [] 
  let seen: Set<Pos> = set [startPos]
  let rec loop (fs: Set<Pos>) (e: (Pos * int) list) (seen: Set<Pos>) = 
    if fs.IsEmpty then e else          
      let p,c = fq.Dequeue()
      let fs' = fs.Remove(p)
      let e' = (p,c) :: e
      let seen' = seen.Add p
      let fs'' =
        Grid.validMovePointer {grid with searchPoint = Some p} 
        |> List.fold (fun (fs'': Set<Pos>) (a,s) ->
          let newP = s.searchPoint.Value
          if not (seen'.Contains newP || fs''.Contains newP) 
          then 
            fq.Enqueue((newP,c+1))
            Set.add newP fs''
          else fs'') fs'
      loop fs'' e' seen'
  loop fs e seen

let getPositions (grid: Grid) =
  grid.dynamicGrid
  |> Map.toArray
  |> Array.filter (fun (_, o) ->
    match o with
    | Agent _ -> true
    | _ -> false)
  |> Array.Parallel.map fst
  |> fun x -> eprintfn "%i" x.Length; x
  |> Array.Parallel.collect (fun p ->
    searchAllPositions grid p
    |> List.toArray
    |> Array.Parallel.map fst)
  |> Array.distinct
  |> Array.Parallel.collect (fun p ->
    searchAllPositions grid p
    |> List.toArray
    |> Array.Parallel.collect (fun (p',c) -> [|((p,p'),c);((p',p),c)|])
  )
  |> Map.ofArray
  

let pickBox ((goalPos, gt): Pos * Goal) (grid: Grid): (Box * Pos) * Pos list = 
    // let grid' = grid
            //    |> Grid.removeAgents
            //    |> Grid.filterDynamicObjects (fun _ -> isOfTypeIfBox gt)
    match graphSearch (BoxPointerProblem (grid, goalPos, gt)) with
        | Some s -> 
            let state = s.Head.state
            let boxPos = state.searchPoint.Value
            match Map.find boxPos state.dynamicGrid with
            | Box box -> (box, boxPos), List.map (fun n -> n.state.searchPoint.Value) s
            | _ -> failwith "search should always lead to a box..." 
        | None -> failwith "NoGoalToBoxPathFound"

let pickAgent ((boxPos, boxColor): Pos * Color) (grid: Grid): (Agent * Pos) * Pos list = 
    // let grid' = grid
            //    |> Grid.filterDynamicObjects (fun _ -> not << isBox)
            //    |> Grid.filterAgents (snd >> (=)boxColor)
    match graphSearch (AgentPointerProblem (grid, boxPos, boxColor)) with
        | Some s -> 
            let state = s.Head.state
            let agentPos = state.searchPoint.Value
            match Map.find agentPos state.dynamicGrid with
            | Agent agent -> (agent, agentPos), List.map (fun n -> n.state.searchPoint.Value) s
            | _ -> failwith "search should always lead to a box..." 
        | None -> failwith "NoGoalToBoxPathFound"

// Its an obstacle if the agent cannot remove it itself
let getObstacleFromPath (agentColor: Color) (state: Grid) (path: Pos list): Pos option = 
    path 
    |> List.tryFind (fun pos -> 
        match Map.find pos state.dynamicGrid with 
        | Box box -> agentColor <> getBoxColor box
        | Agent _ -> true
        | _ -> false)

// pos of all DEmpty pos
let freePos (state: Grid): Set<Pos> =
    Map.toSeq state.dynamicGrid
    |> Seq.filter (fun (pos, t) -> match t with | DEmpty -> true | _ -> false)
    |> Seq.map fst
    |> Set.ofSeq

let getActionsAndResultingState (solution: Node<Grid,Action []> list) = 
    List.rev (List.map (fun n -> n.action) solution), solution.Head.state

let formatPositions (g: Grid) (points: Set<Pos>) = 
    Grid.GridToStringTransformer (fun g (i,j) ->
        if points.Contains (i,j) 
        then "*" 
        else Grid.PosToString g (i,j)) g
    
let formatPath (g: Grid) (path: Pos list) = Set.ofList path |> formatPositions g

let rec solveObstacle prevH (reservedPath: Set<Pos>) (pos: Pos) (state: Grid) = 
    let free = freePos state
    let goalPositions = Set.intersect free reservedPath
                        |> Set.difference free
    match Map.find pos state.dynamicGrid with
    | Box box -> 
        let boxPos = pos
        let goalTest agentIdx s = 
            let agentPos = Map.find agentIdx s.agentPos
            let boxPos = Map.find (getId box) s.boxPos
            goalPositions.Contains agentPos && goalPositions.Contains boxPos

        eprintfn "Removing box: %O" boxPos
        eprintfn "Free positions:"
        formatPositions state goalPositions |> eprintfn "%O"
        let box, agent, (grid', actions) = createClearPathFromBox prevH (box, boxPos) [] state
        match new AStarSokobanProblem (getId box, getAgentIdx agent, grid', prevH, goalTest) |> graphSearch with
        | Some solution -> getActionsAndResultingState solution
        | None -> failwith "come on"
    | Agent a -> 
        let goalTest agentIdx s = 
            let agentPos = Map.find agentIdx s.agentPos
            goalPositions.Contains agentPos
        BFSSokobanProblem (getAgentIdx a, state, goalTest) 
                 |> graphSearch
                 |> Option.get
                 |> getActionsAndResultingState
    | _ -> failwith "Pos should contain Box or Agent obstacle"

// boxPos and agentPos might change during the recursion.. which is not intended
and createClearPathFromBox prevH (box, boxPos) goalPath grid = 
    let (agent, agentPos), boxPath = pickAgent (boxPos, getBoxColor box) grid
    let solutionPath = boxPath @ goalPath |> List.tail // Drops Agent pos
    let solutionSet = Set.ofList solutionPath
    
    eprintfn "Path to be cleared:"
    formatPath grid solutionPath |> eprintfn "%O"
    eprintfn "Picked agent: %O" (getAgentIdx agent)

    let rec clearPath gridAcc solutionAcc = 
        match getObstacleFromPath (getAgentColor agent) gridAcc solutionPath with
        | Some obstacle -> 
            let obsActionSolution, gridAcc' = solveObstacle prevH (solutionSet) obstacle gridAcc
            clearPath gridAcc' (solutionAcc @ obsActionSolution)
        | None -> 
            gridAcc, solutionAcc
    let grid', actions = clearPath grid []
    // eprintfn "%O" grid'
    // eprintfn "%A" (toOutput actions)
    box, agent, (grid', actions)

and createClearPath prevH (goalPos, goal) grid = 
    // eprintfn "createClearPath: %O\n" (goalPos, goal, grid)
    let (box, boxPos), goalPath = pickBox (goalPos, goal) grid
    eprintfn "Solving goal: %O" goal
    eprintfn "Picked box at: %O" boxPos
    createClearPathFromBox prevH (box, boxPos) goalPath grid

// Clear path
// Add cost to box on path in PointerSearch
// Generalize multiagent clearPath to singleagent
// Fix all pairs shortest path preprocessing
// Merge multiagent
// 

let solveGoal (goalPos, goal) prevH grid : Action [] list * Grid = 
        let box, agent, (grid', actions) = createClearPath prevH (goalPos, goal) grid
        match new AStarSokobanProblem (goalPos, getId box, getAgentIdx agent, grid', prevH) |> graphSearch with
        | Some solution -> 
            let state = solution.Head.state.AddWall goalPos
            actions @ List.rev (List.map (fun n -> n.action) solution), state
        | None -> failwith "come on"

let rec solveGoals actions prevH grid = function 
    | [] -> actions
    | goal :: goals -> 
        let actions', grid' = solveGoal goal prevH grid
        eprintfn "solved goal %O:\n%O" goal grid'
        solveGoals (actions @ actions') prevH grid' goals 

let testGoalOrdering grid = 
    eprintfn "Ordering goals"
    let goals = orderGoals grid (Set.ofList (getGoals grid))
    eprintfn "Goal order: %s" ((List.map (snd >> string) goals) |> String.concat ",") 
    eprintfn "Precomputing h values"
    let prevH = getPositions grid
    eprintfn "Solving goals"
    solveGoals [] prevH grid goals |> toOutput |> printOutput

let isOfTypeIfBox gt d = (((not << isBox) d) || (isBoxOfType gt d))

let rec readInput() =
    seq {
        let o = Console.ReadLine()
        yield o
        if o <> "" then yield! readInput()
    }

[<EntryPoint>]
let main args =
    let lines = readInput ()
    // let lines = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__,"levels/testlevels/","MAAgentBoxObstacle.lvl"))
    //let lines = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__,"levels","SAAnagram.lvl"))
    //let lines = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__,"levels","SAsokobanLevel96.lvl"))
    //let lines = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__,"levels","testlevels","competition_levelsSP17","SABlinky.lvl"))
    let grid = getGridFromLines (Seq.toList lines)
    testGoalOrdering grid |> ignore
    0
