// Learn more about F# at http://fsharp.org
module Program
open System
open System.IO
open Domain
open Grid
open Search
open Problem

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
      | Agent(_,c) -> Set.add c s
      | _ -> s
    ) Set.empty
  {grid with 
    dynamicGrid = 
      grid.dynamicGrid
      |> Map.map (fun _ o -> 
        match o with
        | Box(_,_,c) when agentColors.Contains c |> not -> Wall
        | x -> x
      )}

let getGrid filename =
  let lines = Path.Combine(__SOURCE_DIRECTORY__, filename) |> readLines
  getGridFromLines lines

let formatPositions (g: Grid) (points: Set<Pos>) = 
    Grid.GridToColoredStringTransformer (fun g (i,j) ->
        let char = Grid.PosToColoredString g (i,j)
        if points.Contains (i,j) 
        then {char with background = ConsoleColor.Gray;}
        else char) g
    
let formatPath (g: Grid) (path: Pos list) = Set.ofList path |> formatPositions g

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
        
    let prevH = getPositions grid
    let grid = removeUnmovableBoxes grid prevH
    let isMA = grid.agentPos.Count > 1

    let goals = orderGoals grid prevH isMA boxTypeToId agentColorToId (grid.GetGoals ())
    let actions, _ = solveGoals prevH boxTypeToId agentColorToId grid goals
    
    actions 
    |> makeConcurrent 
    |> Array.map fst 
    |> toOutput 
    |> printOutput

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
