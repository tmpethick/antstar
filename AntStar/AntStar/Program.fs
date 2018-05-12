// Learn more about F# at http://fsharp.org
module Program
open System
open System.IO
open Domain
open Grid
open Search
open System.Collections.Generic

let rec toOutput (l: Action list): string list =
    match l with
    | []                -> []
    | NOP::t            -> "[NoOp]"::toOutput t
    | Move (_,d)::t     -> "[Move("+string d+")]"::toOutput t
    | Pull(_,ad,bd)::t  -> "[Pull("+string ad+","+string bd+")]"::toOutput t
    | Push(_,ad,bd)::t  -> "[Push("+string ad+","+string bd+")]"::toOutput t
    | MovePointer(_)::t -> toOutput t
    
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
  

let pickBox ((goalPos, gt): Pos * Goal) (grid: Grid): Context<Pos * Box> = 
    let grid' = grid
            //    |> Grid.removeAgents
            //    |> Grid.filterDynamicObjects (fun _ -> isOfTypeIfBox gt)
    match graphSearch (BoxPointerProblem (grid', goalPos, gt)) with
        | Some s -> 
            let state = s.Head.state
            let boxPos = state.searchPoint.Value
            match Map.find boxPos state.dynamicGrid with
            | Box box -> Success (boxPos, box)
            | _ -> failwith "search should always lead to a box..." 
        | None -> Error NoGoalToBoxPathFound

let pickAgent ((boxPos, boxColor): Pos * Color) (grid: Grid): Context<Pos * Agent> = 
    let grid' = grid
            //    |> Grid.filterDynamicObjects (fun _ -> not << isBox)
            //    |> Grid.filterAgents (snd >> (=)boxColor)
    match graphSearch (AgentPointerProblem (grid', boxPos, boxColor)) with
        | Some s -> 
            let state = s.Head.state
            let agentPos = state.searchPoint.Value
            match Map.find agentPos state.dynamicGrid with
            | Agent agent -> Success (agentPos, agent)
            | _ -> failwith "search should always lead to a box..." 
        | None -> Error NoGoalToBoxPathFound

let solveGoal (goalPos, goal) prevH grid : Action list * Grid = 
        let boxPos, box = pickBox (goalPos, goal) grid |> unWrap
        let agentPos, agent = pickAgent (boxPos, getBoxColor box) grid |> unWrap
        match AStarSokobanProblem (goalPos, getId box, getAgentIdx agent, grid, prevH) |> graphSearch with
        | Some solution -> 
            let state = solution.Head.state.AddWall goalPos
            solution |> List.map (fun n -> n.action), state
        | None -> failwith "come on"

let rec solveGoals actions prevH grid = function 
    | [] -> actions
    | goal :: goals -> 
      let actions', grid' = solveGoal goal prevH grid
      solveGoals (actions @ List.rev actions') prevH grid' goals 

let testGoalOrdering grid = 
    eprintfn "Ordering goals"
    let goals = orderGoals grid (Set.ofList (getGoals grid))
    eprintfn "Goal order: %s" (goals |> List.map snd |> List.map (fun x -> string x) |> String.concat ",") 
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
    //let lines = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__,"levels","SAsokobanLevel96.lvl"))
    //let lines = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__,"levels","testlevels","competition_levelsSP17","SABlinky.lvl"))
    let grid = getGridFromLines (Seq.toList lines)
    testGoalOrdering grid |> ignore
    0
