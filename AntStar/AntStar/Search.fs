module Search

open System
open System.IO
open FSharpx.Collections
open Grid 
open Domain 

let getAgentPos (g: Grid) = (Seq.head g.agentPos).Value

let isBoxOfType    t = function | Box (_,t', _)   -> t' = t | _ -> false
let isAgentOfColor c = function | Agent (_, c') -> c' = c | _ -> false

let isBox = function | Box _ -> true | _ -> false

let findAdjecent (predicate: DynamicObject -> bool) (pos: Pos) (grid: Grid) : (Pos * DynamicObject) option = 
    [N;S;E;W]
    |> List.map (flip posFromDir pos)
    |> List.map (fun pos -> pos, grid.dynamicGrid.TryFind pos)
    |> List.tryFind (function
                     | (pos, Some o) when predicate o -> true
                     | _ -> false)
    |> Option.map (function | (p, Some o) -> (p, o) | _ -> failwith "expected DynamicObject to be Some")

// let pickBox ((goalPos, objtype): Pos * Goal) (grid: Grid): Context<Pos * Box> = 
//    let agent = defaultAgent
//    let grid' = grid
//                |> Grid.removeAgents
//                |> Grid.addAgent goalPos agent
//    let isGoal g = 
//            let agentPos = getAgentPos g
//            Option.isSome (findAdjecent (isBoxOfType objtype) agentPos g)
//    match graphSearch (SokobanProblem (grid', goalPos, objtype)) with
//    | None -> Error NoGoalToBoxPathFound
//    | Some solution -> 
//        let endState = (List.head solution).state
//        let agentPos = getAgentPos endState
//        match findAdjecent (isBoxOfType objtype) agentPos endState with
//        | Some (pos, Box b) -> Success (pos, b)
//        | _ -> failwith "If a solution is found a box MUST be adjecent. Otherwise you're in dead trouble."


[<AbstractClass>]
type Problem<'s, 'a> () =
   abstract Initial : 's
   abstract member Actions: 's -> list<'a * 's>
   abstract member GoalTest: 's -> bool
   abstract member ChildNode: Node<'s,'a> -> 'a -> 's -> Node<'s,'a>
   abstract member initialAction: unit -> 'a

let root (p: Problem<'s,'a>): Node<'s,'a> = 
        let s = p.Initial
        { state  = s;
          parent = None;
          action = p.initialAction ();
          cost   = 0;
          value  = 0}

type SearchQueue<'s,'a when 's: comparison> = 
    {q: IPriorityQueue<Node<'s,'a>>; m: Map<'s,Node<'s,'a>> }
    static member empty = {q = PriorityQueue.empty false; m = Map.empty; }
    member pq.IsEmpty = pq.q.IsEmpty
    member pq.Pop = let el, q' = PriorityQueue.pop pq.q
                    el, {pq with q = q'; m = pq.m.Remove el.state}
    member pq.Insert el = {pq with q = PriorityQueue.insert el pq.q;
                                  m = pq.m.Add (el.state, el)}
    member pq.Contains state = pq.m.ContainsKey state
    member pq.TryFind state = pq.m.TryFind state
    member pq.Length = pq.q.Length

[<AbstractClass>]
type ISokobanProblem () =
   inherit Problem<Grid,Action []>()

type PointerProblem (grid: Grid, startPos: Pos, goalTest: Pos -> Grid -> Boolean) = 
    inherit ISokobanProblem()
    override p.Initial = {grid with searchPoint = Some startPos }
    override p.Actions s = Grid.validMovePointer s |> List.map (fun (a,g) -> (Array.create 1 a, g))
    override p.GoalTest s = match s.searchPoint with 
                            | Some pointer -> goalTest pointer s
                            | None -> false
    override p.ChildNode n a s = 
        let n' = gridToNode n a s
        let additionCost = 
            match s.searchPoint with 
            | None -> 0
            | Some pointer -> 
                match Map.find pointer s.dynamicGrid with
                | Agent _ -> 3
                | Box _ -> 10
                | _ -> 0
        { n' with cost = n'.cost + additionCost; value = n'.value + additionCost }
        
    override p.initialAction () = [|NOP|]
let manhattanDistance p1 p2 = abs (fst p1 - fst p2) + abs (snd p1 - snd p2)
let euclideanDistance (p1: Pos) (p2: Pos) = 
    let e1 = (float (fst p1 - fst p2) ** 2.0)
    let e2 = (float (snd p1 - snd p2) ** 2.0)
    Math.Sqrt (e1 + e2) |> int
type PosPointerProblem (grid, startPos, endPos) =
    inherit PointerProblem(grid, startPos, fun p -> fun _ -> p = endPos)

    override p.ChildNode n a s = 
      let child = gridToNode n a s
      {child with value = child.cost + euclideanDistance child.state.searchPoint.Value endPos}

let boxGoalTest goal pointer s = s.dynamicGrid.TryFind pointer |> Option.exists (isBoxOfType goal)
type BoxPointerProblem (grid, startPos, goal, prevH: Map<Pos*Pos,int>, boxPositions: Set<Pos>) =
  inherit PointerProblem(grid, startPos, 
    fun p -> fun _ -> boxPositions |> Set.contains p)

  override p.ChildNode n a s = 
      let child = gridToNode n a s
      let h = 
        boxPositions
        |> Set.map (fun p -> prevH.[child.state.searchPoint.Value,p])
        |> Set.minElement
      {child with value = child.cost + h}

let agentGoalTest color pointer s = Option.exists (isAgentOfColor color) (s.dynamicGrid.TryFind pointer)
type AgentPointerProblem (grid, startPos, color, prevH: Map<Pos*Pos,int>, agentColorToId: Set<Pos>) =
  inherit PointerProblem(grid, startPos, 
    fun p -> fun _ -> agentColorToId |> Set.contains p)

  override p.ChildNode n a s = 
      let child = gridToNode n a s
      let h = 
        agentColorToId
        |> Set.map (fun p -> prevH.[child.state.searchPoint.Value,p])
        |> Set.minElement
      {child with value = child.cost + h}

let allGoalsMet (grid: Grid) = 
    grid.staticGrid
     |> Map.forall (fun gpos e -> 
        match e with 
        | SEmpty -> true
        | Goal gt -> 
            match grid.dynamicGrid.TryFind gpos with
            | Some (Box (_,bt, _)) -> gt = bt
            | _ -> false)


let rec retrieveSolution (n: Node<'s,'a>) = 
    match n.parent with
    | None    -> []
    | Some n' -> n :: retrieveSolution n'

let graphSearch (p: ISokobanProblem) = 
    let initialEl = root p
    let f = (SearchQueue<Grid,Action []>.empty).Insert initialEl
    let e: Set<Grid> = Set.empty
    let rec loop (e: Set<Grid>) (f: SearchQueue<Grid,Action []>) = 
        // let frontierStates = Map.map (fun s n -> n.state) f.m
        // printfn "%O" frontierStates
        // eprintfn "Size of explored: %O\n" e.Count
        if f.IsEmpty
        then None
        else 
            let n, f' = f.Pop
            // n.state.ToColorRep() |> cprintLines
            if p.GoalTest n.state
            then 
                Some (retrieveSolution n)
            else 
                let e' = e.Add n.state
                let f'' = p.Actions n.state |> List.fold (fun (f'': SearchQueue<Grid,Action []>) (a,s) ->
                    let c = p.ChildNode n a s
                    let isNew = not ((e'.Contains c.state) || (f''.Contains c.state))
                    let isCheaper = 
                        match f''.TryFind c.state with
                        | Some n -> n.value > c.value
                        | None -> false
                    if isNew || isCheaper 
                    then f''.Insert c
                    else f'') f'
                loop e' f''
    loop e f

// TODO: multiple goals can use same box. Might lead ordering with non-trivial solution.
let rec orderGoals' (grid: Grid) (prevH: Map<Pos * Pos, int>) (boxTypeToId: Map<ObjType,Set<Guid>>) (orderedGoals : List<Pos * Goal>) (unsolvedGoals : Set<Pos * Goal>) = 
    // set other unsolvedGoals to walls
    // search for box of Goal type
    // return true if exists
    let isSolvableGoal ((goalPos, gt): Pos * Goal) = 
        let grid' = 
          grid
          |> Grid.removeAgents
          |> Grid.filterDynamicObjects (fun _ d -> (((not << isBox) d) || (isBoxOfType gt d)))
          |> flip (Set.fold (fun g (pos, _) -> g.AddWall pos)) (unsolvedGoals.Remove (goalPos, gt))

        // printfn "%O" grid'
        match graphSearch (BoxPointerProblem (grid', goalPos, gt, prevH, boxTypeToId.[gt] |> Set.map (fun id -> grid'.boxPos.[id]))) with
        | Some _ -> true
        | None -> false  
    
    if unsolvedGoals.IsEmpty 
        then orderedGoals
        else match Set.toList unsolvedGoals |> List.tryFind isSolvableGoal with
             | Some g -> orderGoals' grid prevH boxTypeToId (g :: orderedGoals) (unsolvedGoals.Remove g)
             | None -> failwith (sprintf "%A are unsolvable" unsolvedGoals)

let orderGoals grid prevH boxTypeToId unsolvedGoals = orderGoals' grid prevH boxTypeToId [] unsolvedGoals

type BFSSokobanProblem(agentIdx: AgentIdx, grid: Grid, goalTest) = 
    inherit ISokobanProblem()
    let numAgents = grid.agentPos.Count
    override p.Initial = grid
    override p.Actions s = Grid.validSokobanActions numAgents agentIdx s
    override p.GoalTest s = goalTest agentIdx s
    override p.ChildNode n a s = 
      let child = gridToNode n a s
      {child with value = child.cost}
    override p.initialAction () = Array.create numAgents NOP

// AStarSokobanProblem(boxGuid: Guid, agentIdx: AgentIdx, grid: Grid, prevHValues: Map<Pos*Pos, int>, goalTest)
type AStarSokobanProblem (boxGuid: Guid, agentIdx: AgentIdx, grid: Grid, prevHValues: Map<Pos*Pos, int>, goalTest, heuristicTransformer) =
    inherit ISokobanProblem()

    let numAgents = grid.agentPos.Count
    override p.Initial = grid
    override p.Actions s = Grid.validSokobanActions numAgents agentIdx s
    override p.GoalTest s = goalTest agentIdx s
                            
    override p.ChildNode n a s = 
        let boxPos = Map.find boxGuid s.boxPos

        let agentPos = Map.find agentIdx s.agentPos
        let agentH = Map.find (boxPos,agentPos) prevHValues
        let node = gridToNode n a s
        let h = 8*agentH
        {node with value = node.cost + heuristicTransformer boxPos h }
        //{node with value = node.cost + manhattanDistance goalPos boxPos + manhattanDistance boxPos agentPos}
    override p.initialAction () = Array.create numAgents NOP

    /// Heuristic search with goal.
    new ((goalPos,nextGoalPos): Pos * (Pos option), boxGuid: Guid, agentIdx: AgentIdx, grid: Grid, prevHValues: Map<Pos*Pos, int>) = 
        let goalTest agentIdx s = 
            let boxPos = Map.find boxGuid s.boxPos
            let agentPos = Map.find agentIdx s.agentPos
            let onGoal = false
                //match Map.find agentPos s.staticGrid with
                //| Goal _ -> true
                //| _ -> false
            match (goalPos = boxPos) && not onGoal with
            | false -> false
            | true ->
              match Option.isSome nextGoalPos with
              | false -> true
              | true ->
                PosPointerProblem(grid.AddWall(goalPos), agentPos, nextGoalPos.Value)
                |> graphSearch
                |> Option.isSome
        let heuristicTransformer boxPos h = 
            let boxH = Map.find (goalPos,boxPos) prevHValues
            h + 10*boxH
        AStarSokobanProblem (boxGuid, agentIdx, grid, prevHValues, goalTest, heuristicTransformer)

    /// Heuristic search without goal.
    new (boxGuid: Guid, agentIdx: AgentIdx, grid: Grid, prevHValues: Map<Pos*Pos, int>, goalTest) =
        AStarSokobanProblem (boxGuid, agentIdx, grid, prevHValues, goalTest, fun _ h -> h)
