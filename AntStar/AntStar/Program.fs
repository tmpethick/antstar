// Learn more about F# at http://fsharp.org
module Program
open System
open System.IO
open Domain
open Grid
open Search
open System.Collections.Generic

type ActionMeta = Action [] * LockedPos
type ActionList = ActionMeta list

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
    //eprintfn "%s" s
    s

let rec toOutput (l: (Action []) []): string [] = Array.map convertActionArray l
    
let rec printOutput (l:string []) = Array.iter (printfn "%s") l

let rec testActions state = function
    | action :: actions ->
        match apply action state with
        | Error err      -> failwith (err.ToString ())
        | Success state' -> printfn "%O" state'
                            testActions state' actions
    | [] -> printfn "Is Goal state: %O" (allGoalsMet state)

let getGridFromLines lines : Grid = 
  let colors, gridLines = parseColors Map.empty (lines)
  let grid = parseMap colors (gridLines |> addIdx)
  let agentColors = 
    grid.dynamicGrid
    |> Map.fold (fun s p o ->
      match o with
      | Agent(id,c) -> Set.add c s
      | _ -> s
    ) Set.empty
  {grid with 
    dynamicGrid = 
      grid.dynamicGrid
      |> Map.map (fun _ o -> 
        match o with
        | Box(id,t,c) when agentColors.Contains c |> not -> Wall
        | x -> x
      )}

let getGrid filename =
  let lines = Path.Combine(__SOURCE_DIRECTORY__, filename) |> readLines
  getGridFromLines lines

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
  |> Array.Parallel.collect (fun (p,_) ->
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
  

let pickBox ((goalPos, gt): Pos * Goal) (prevH: Map<Pos*Pos,int>) (boxTypeToId: Map<ObjType, Set<Guid>>) (grid: Grid) : (Box * Pos) * Pos list = 
    // let grid' = grid
            //    |> Grid.removeAgents
            //    |> Grid.filterDynamicObjects (fun _ -> isOfTypeIfBox gt)
    match graphSearch (BoxPointerProblem (grid, goalPos, gt, true, prevH, boxTypeToId.[gt] |> Set.map (fun id -> grid.boxPos.[id]))) with
        | Some [] ->
          match Map.find goalPos grid.dynamicGrid with
          | Box box -> (box, goalPos), []
          | _ -> failwith "search should always lead to a box..." 
        | Some s -> 
            let state = s.Head.state
            let boxPos = state.searchPoint.Value
            match Map.find boxPos state.dynamicGrid with
            | Box box -> (box, boxPos), List.append (List.map (fun n -> n.state.searchPoint.Value) s) [goalPos]
            | _ -> failwith "search should always lead to a box..." 
        | None -> failwith "NoGoalToBoxPathFound"

let pickAgent ((boxPos, boxColor): Pos * Color) (prevH: Map<Pos*Pos,int>) (agentColorToId: Map<Color,Set<AgentIdx>>) (grid: Grid): (Agent * Pos) * Pos list = 
    // let grid' = grid
            //    |> Grid.filterDynamicObjects (fun _ -> not << isBox)
            //    |> Grid.filterAgents (snd >> (=)boxColor)
    match graphSearch (AgentPointerProblem (grid, boxPos, boxColor, true, prevH, agentColorToId.[boxColor] |> Set.map (fun id -> grid.agentPos.[id]))) with
        | Some s -> 
            let state = s.Head.state
            let agentPos = state.searchPoint.Value
            match Map.find agentPos state.dynamicGrid with
            | Agent agent -> (agent, agentPos), List.map (fun n -> n.state.searchPoint.Value) s
            | _ -> failwith "search should always lead to an agent..." 
        | None -> // grid.ToColorRep () |> cprintLines
                  failwith "NoBoxToAgentPathFound"

// Its an obstacle if the agent cannot remove it itself
let getObstacleFromPath (agentIdx: AgentIdx) (protectedBox: Box option) (state: Grid) (path: Pos list): Pos option = 
    path 
    |> List.tryFind (fun pos -> 
        match Map.find pos state.dynamicGrid with 
        | Box box -> 
            match protectedBox with
            | Some b' -> box <> b'
            | None -> true
        | Agent a -> 
            agentIdx <> getAgentIdx a
        | _ -> false)

// pos of all DEmpty pos
let freePos (state: Grid): Set<Pos> =
    Map.toSeq state.dynamicGrid
    |> Seq.filter (fun (pos, t) -> match t with | DEmpty -> true | _ -> false) // TODO:? relax to isWall |> not)
    |> Seq.map fst
    |> Set.ofSeq

let getActionsAndResultingState (solution: Node<Grid,Action []> list) = 
    List.rev (List.map (fun n -> n.action) solution), solution.Head.state

let getActionsAndResultingState' (solution: Node<Grid,ActionMeta> list): (ActionMeta) list * Grid = 
    let actions = List.map (fun n -> n.action) solution |> List.rev
    actions, solution.Head.state

let formatPositions (g: Grid) (points: Set<Pos>) = 
    Grid.GridToColoredStringTransformer (fun g (i,j) ->
        let char = Grid.PosToColoredString g (i,j)
        if points.Contains (i,j) 
        then {char with background = ConsoleColor.Gray;}
        else char) g
    
let formatPath (g: Grid) (path: Pos list) = Set.ofList path |> formatPositions g

let rec solveObstacle prevH agentColorToId (reservedPath: Set<Pos>) (pos: Pos) (state: Grid): ActionList * Grid = 
    let free = freePos state
    let freePositions = Set.intersect free reservedPath
                        |> Set.difference free

    match Map.find pos state.dynamicGrid with
    | Box box -> 
        let boxPos = pos

        let boxType = (getType box).ToString().ToUpper()
        // eprintfn "Solving obstacle %O at %O" boxType boxPos
        // formatPositions state freePositions |> cprintLines

        let goalTest agentIdx s = 
            let agentPos = Map.find agentIdx s.agentPos
            let boxPos = Map.find (getId box) s.boxPos
            freePositions.Contains agentPos && freePositions.Contains boxPos

        let box, agent, (grid', actions) = createClearPathFromBox prevH agentColorToId (box, boxPos) [] state
        match new AStarSokobanProblem (getId box, getAgentIdx agent, grid', prevH, goalTest) |> graphSearch with
        | Some solution -> 
            let acts, s = getActionsAndResultingState' solution
            // s.ToColorRep () |> cprintLines
            actions @ acts, s
        | None -> failwith "come on"
    | Agent a -> 
        // TODO: clear agent path with createClearPathFromBox
        //eprintfn "Solving obstacle %O at %O" (getAgentIdx a) pos
        // formatPositions state freePositions |> cprintLines

        let goalTest agentIdx s = 
            let agentPos = Map.find agentIdx s.agentPos
            let onGoal =
                match Map.find agentPos s.staticGrid with
                | Goal _ -> true
                | _ -> false
            freePositions.Contains agentPos && not onGoal
            
        let grid', actions = createClearPathForAgent prevH agentColorToId a freePositions state
        //eprintfn "Solving obstacle %O at %O now cleared" (getAgentIdx a) pos
        // formatPositions grid' freePositions |> cprintLines

        let acts, grid'' = 
            BFSSokobanProblem (getAgentIdx a, grid', goalTest) 
            |> graphSearch
            |> Option.get
            |> getActionsAndResultingState'
        actions @ acts, grid''

    | _ -> failwith "Pos should contain Box or Agent obstacle"

// boxPos and agentPos might change during the recursion.. which is not intended
and createClearPathFromBox prevH (agentColorToId: Map<Color,Set<AgentIdx>>) (box, boxPos) goalPath grid = 
    let (agent, agentPos), boxPath = pickAgent (boxPos, getBoxColor box) prevH agentColorToId grid
    let solutionPath = boxPath @ goalPath |> List.tail // Drops Agent pos
    let solutionSet = Set.ofList solutionPath
    
    // formatPath grid solutionPath |> cprintLines

    box, agent, clearPath prevH agentColorToId agent (Some box) solutionPath grid

and createClearPathForAgent prevH agentColorToId agent freeSpots grid =
    let agentPos = Map.find (getAgentIdx agent) grid.agentPos
    let agentPath = 
        match FreeSpotPointerProblem (grid, agentPos, freeSpots) |> graphSearch with
        | Some s -> List.map (fun n -> n.state.searchPoint.Value) s
        | None -> failwith "could not clear agent"
        
    // formatPath grid agentPath |> cprintLines

    clearPath prevH agentColorToId agent None agentPath grid
    
and clearPath (prevH: Map<(Pos * Pos),int>) (agentColorToId: Map<Color,Set<AgentIdx>>) agent (box: Box option) solutionPath grid =
    let solutionSet = Set.ofList solutionPath
    let rec clearPath' gridAcc solutionAcc = 
        match getObstacleFromPath (getAgentIdx agent) box gridAcc solutionPath with
        | Some obstacle -> 
            let obsActionSolution, gridAcc' = solveObstacle prevH agentColorToId (solutionSet) obstacle gridAcc
            // eprintfn "After removing obstacle"
            // gridAcc'.ToColorRep() |> cprintLines
            clearPath' gridAcc' (solutionAcc @ obsActionSolution)
        | None -> 
            gridAcc, solutionAcc
    let grid', actions = clearPath' grid []
    grid', actions

and createClearPath prevH boxTypeToId agentColorToId (goalPos, goal) grid = 
    // eprintfn "createClearPath: %O\n" (goalPos, goal, grid)
    let (box, boxPos), goalPath = pickBox (goalPos, goal) prevH boxTypeToId grid
    // eprintfn "Picked box at: %O" boxPos
    createClearPathFromBox prevH agentColorToId (box, boxPos) goalPath grid

let solveGoal (goalPos, goal) (goals: (Pos * Goal) list) prevH boxTypeToId agentColorToId grid : ActionList * Grid = 
        //eprintfn "solving goal %O:" goal
        // eprintfn "with pos: %O" goalPos
        let box, agent, (grid', actions) = createClearPath prevH boxTypeToId agentColorToId (goalPos, goal) grid
        let boxType = (getType box).ToString().ToUpper()
        //eprintfn "Path cleared for %O, %O, %O" goal boxType (getAgentIdx agent)
        
        let nextGoalPos =  None

        match new AStarSokobanProblem ((goalPos,nextGoalPos), getId box, getAgentIdx agent, grid', prevH) |> graphSearch with
        | Some [] ->
          actions,grid'
        | Some solution ->
            // solution.Head.state.ToColorRep() |> cprintLines
            let actions', grid = getActionsAndResultingState' solution
            (actions @ actions'), grid
        | None -> failwith "come on"

let getUnsolvedGoals (expectedSolvedGoals: Set<Pos * Goal>) (grid: Grid) = 
    grid.GetGoals ()
    |> Set.filter (fun (pos, goal) ->
        Set.contains (pos,goal) expectedSolvedGoals
        && not (grid.dynamicGrid.[pos] |> isBoxOfType goal)
    )

let accLockedFields (prefix: (Action [] * LockedPos) list): (Action [] * HistoryLockedPos) list = 
    prefix
    |> List.map snd
    |> List.rev
    |> List.scan Set.union Set.empty
    |> List.tail
    |> List.rev
    |> List.zip (prefix |> List.map fst)

/// Expects `arr` to be sorted in decending order (decreasing size of set).
let binSearch<'a when 'a: comparison> (target: Set<'a>) (arr: (Action [] * Set<'a>) []) = 
    let rec binSearch' currentBest lo hi =
        if lo <= hi then
            let mid = lo + ((hi - lo) >>> 1)
            let intersection = Set.intersect target (snd arr.[mid])
            // eprintfn "Binary search intersection: %O" intersection.Count
            // eprintfn "Current best: %O" currentBest
            if Set.isEmpty intersection
            then binSearch' (Some mid) lo (mid - 1) // left is bigger
            else binSearch' currentBest (mid + 1) hi  // right is smaller
        else currentBest
    binSearch' None 0 (Array.length arr - 1)

let mergeAction startIdx (actions: (Action [] * HistoryLockedPos) []) (action: ActionMeta) =
    let mergeActions (a1: Action []) (a2: Action []) = 
        Array.zip a1 a2
        |> Array.map (fun (a1,a2) -> 
            match a1 with
            | NOP -> a2
            | v -> v)

    let mergeActionMeta (a1,l1) (a2,l2) = (mergeActions a1 a2), Set.union l1 l2

    let left, right = 
        match startIdx with
        | Some idx -> Array.splitAt idx actions
        | None     -> actions, [||]
    
    let middle, right = 
        if Array.isEmpty right then
            action, [||]
        else 
            (mergeActionMeta (right.[0]) action), right.[1..]

    let left = 
        let _, locked = action
        left |> Array.map (fun (a, l) -> a, Set.union l locked)

    Array.concat [left; [|middle|]; right]

let makeConcurrent actions = 
    let merge actionsAcc action =
        let startIdx = binSearch (snd action) actionsAcc
        mergeAction startIdx actionsAcc action
    Array.fold merge [||] actions

let rec solveGoals prevH boxTypeToId agentColorToId grid (goalOrder: (Pos * Goal) list) =

    let rec solveGoals' actions grid goals goalOrder = 
        if Set.isEmpty goals
        then actions, grid
        else 
            let goal = goalOrder |> List.find (fun g -> Set.contains g goals)
            let actions', grid = solveGoal goal [] prevH boxTypeToId agentColorToId grid
            let goals = grid.GetUnsolvedGoals ()
            let actions' = actions' |> List.toArray

            // grid.ToColorRep() |> cprintLines
            // eprintfn "Goals left: %O" goals            
            // let goalOrder, _ = orderGoals grid prevH true boxTypeToId agentColorToId goals

            solveGoals' (Array.concat [actions; actions']) grid goals goalOrder
    
    let goals: Set<Pos * Goal> = Set.ofList goalOrder
    
    solveGoals' [||] grid goals goalOrder

let removeUnmovableBoxes (grid: Grid) (prevH: Map<Pos*Pos,int>) =
    let removedBoxes = 
        grid.dynamicGrid 
        |> Map.fold (fun s p o ->
            match o with
            | Box(id, ot, c) ->
                let matchingAgents = 
                    grid.dynamicGrid
                    |> Map.fold (fun ps p' o' ->
                        match o' with
                        | Agent(aid,ac) when c = ac -> Set.add p' ps
                        | _ -> ps
                    ) Set.empty
                let hasPath =
                    matchingAgents
                    |> Set.exists (fun aPos ->
                        Map.containsKey (aPos,p) prevH
                    )
                if hasPath then s else Map.add p Wall s
            | _ -> s
        ) grid.dynamicGrid
    {grid with dynamicGrid = removedBoxes}

let testGoalOrdering (grid: Grid) = 
    //eprintfn "Mapping types to positions"
    let boxTypeToId =
      grid.dynamicGrid
      |> Map.toArray
      |> Array.fold (fun m (x,y) ->
        match y with
        | Box(id,t,c) -> 
          let s': Set<Guid> =
            match Map.tryFind t m with
            | Some s -> Set.add id s
            | None -> Set.singleton id
          Map.add t s' m
        | _ -> m) Map.empty
    
    let agentColorToId =
      grid.dynamicGrid
      |> Map.toArray
      |> Array.fold (fun m (x,y) ->
        match y with
        | Agent(id,c) -> 
          let s': Set<AgentIdx> =
            match Map.tryFind c m with
            | Some s -> Set.add id s
            | None -> Set.singleton id
          Map.add c s' m
        | _ -> m) Map.empty

    //eprintfn "Precomputing h values"
    let prevH = getPositions grid
    // eprintfn "Removing unmovable boxes"
    let grid = removeUnmovableBoxes grid prevH
    // grid.ToColorRep() |> cprintLines
    // eprintfn "Ordering goals"
    //eprintfn "Ordering goals"
    let isMA = grid.agentPos.Count > 1

    let goals = orderGoals grid prevH isMA boxTypeToId agentColorToId (grid.GetGoals ())
    // eprintfn "Goal order: %s" ((List.map (snd >> string) goals) |> String.concat ",") 
    // eprintfn "Solving goals"
    let actions, grid = solveGoals prevH boxTypeToId agentColorToId grid goals
    
    actions 
    |> makeConcurrent 
    |> Array.map fst 
    |> toOutput 
    |> printOutput

    // minimum matching
    // order which is possible

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
    //let lines = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__,"levels","debugging.lvl"))
    //let lines = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__,"levels","competition_levels","MAdashen.lvl"))
    //let lines = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__,"levels","SAsokobanLevel96.lvl"))
    //let lines = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__,"levels","testlevels","competition_levelsSP17","SAEvilCorp.lvl"))
    let grid = getGridFromLines (Seq.toList lines)
    testGoalOrdering grid |> ignore
    0
