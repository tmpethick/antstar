module Search

open System
open System.IO
open FSharpx.Collections
open Grid 
open Domain 

let getAgentPos (g: Grid) = (Seq.head g.agentPos).Value

let isAgentOfColor c = function | Agent (_, c') -> c' = c | _ -> false

let isBox = function | Box _ -> true | _ -> false
let isWall = function | Wall -> true | _ -> false

let findAdjecent (predicate: DynamicObject -> bool) (pos: Pos) (grid: Grid) : (Pos * DynamicObject) option = 
    [N;S;E;W]
    |> List.map (
        flip posFromDir pos 
        >> fun pos -> pos, grid.dynamicGrid.TryFind pos)
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
    static member empty: SearchQueue<'s,'a> = {q = PriorityQueue.empty false; m = Map.empty; }
    member pq.IsEmpty = pq.q.IsEmpty
    member pq.Pop = let el, q' = PriorityQueue.pop pq.q
                    el, {pq with q = q'; m = pq.m.Remove el.state}
    member pq.Insert el = {pq with q = PriorityQueue.insert el pq.q;
                                  m = pq.m.Add (el.state, el)}
    member pq.Contains state = pq.m.ContainsKey state
    member pq.TryFind state = pq.m.TryFind state
    member pq.Length = pq.q.Length


[<AbstractClass>]
type ISokobanProblem<'a> () =
   inherit Problem<Grid,'a>()

let additionalCost s = 
            match s.searchPoint with 
            | None -> 0
            | Some pointer -> 
                match Map.find pointer s.dynamicGrid with
                | Agent _ -> 3
                | Box b -> 
                    match Map.find pointer s.staticGrid with
                    | Goal g when getType b = g -> 100 // If there is no other way
                    | _ -> 10
                | _ -> 0


type PointerProblem (grid: Grid, startPos: Pos, goalTest: Pos -> Grid -> Boolean) = 
    inherit ISokobanProblem<Action []>()
    override p.Initial = {grid with searchPoint = Some startPos }
    override p.Actions s = Grid.validMovePointer s |> List.map (fun (a,g) -> (Array.create 1 a, g))
    override p.GoalTest s = match s.searchPoint with 
                            | Some pointer -> goalTest pointer s
                            | None -> false
    override p.ChildNode n a s = 
        let n' = gridToNode n a s
        let cost' = n'.cost + additionalCost s
        {n' with value = n'.cost; cost = cost'}

    override p.initialAction () = [|NOP|]

let freeSpotGoalTest freePoints pointer s = Set.contains pointer freePoints
type FreeSpotPointerProblem (grid, startPos, freePoints) =
    inherit PointerProblem(grid, startPos, freeSpotGoalTest freePoints)

let manhattanDistance p1 p2 = abs (fst p1 - fst p2) + abs (snd p1 - snd p2)
let euclideanDistance (p1: Pos) (p2: Pos) = 
    let e1 = (float (fst p1 - fst p2) ** 2.0)
    let e2 = (float (snd p1 - snd p2) ** 2.0)
    Math.Sqrt (e1 + e2) |> int
type PosPointerProblem (grid, startPos, endPos, prevH) =
    inherit PointerProblem(grid, startPos, fun p -> fun _ -> p = endPos)

    override p.ChildNode n a s = 
      let child = gridToNode n a s
      let h = 
        match Map.tryFind (child.state.searchPoint.Value, endPos) prevH with
        | Some i -> child.cost + i
        | None -> Cost.MaxValue
      {child with value = h}

let boxGoalTest goal pointer s = s.dynamicGrid.TryFind pointer |> Option.exists (isBoxOfType goal)
type BoxPointerProblem (grid, startPos, goal, addCost: bool, prevH: Map<Pos*Pos,int>, boxPositions: Set<Pos>) =
  inherit PointerProblem(grid, startPos, 
    fun p -> fun _ -> boxPositions |> Set.contains p)

  override p.ChildNode n a s = 
        let n' = gridToNode n a s
        let cost' = if addCost then n'.cost + additionalCost s else n'.cost
        let h = 
            boxPositions
            |> Set.map (fun p -> 
                match Map.tryFind (n'.state.searchPoint.Value,p) prevH with
                | Some v -> v + cost'
                | None -> Cost.MaxValue)
            |> Set.minElement
        {n' with value = h; cost = n'.cost;}
let agentGoalTest color pointer s = Option.exists (isAgentOfColor color) (s.dynamicGrid.TryFind pointer)
type AgentPointerProblem (grid, startPos, color, addCost: bool, prevH: Map<Pos*Pos,int>, agentPositions: Set<Pos>) =
  inherit PointerProblem(grid, startPos, 
    fun p -> fun _ -> agentPositions |> Set.contains p)

  override p.ChildNode n a s = 
      let n' = gridToNode n a s
      let cost' = if addCost then n'.cost + additionalCost s else n'.cost
      let h = 
        agentPositions
        |> Set.map (fun p -> 
          match Map.tryFind (n'.state.searchPoint.Value,p) prevH with
          | Some v -> v + cost'
          | None -> Cost.MaxValue)
        |> Set.minElement
      {n' with value = h; cost = n'.cost;}

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

let graphSearch<'a> (p: ISokobanProblem<'a>) = 
    let initialEl: Node<Grid,'a> = root p
    let f: SearchQueue<Grid,'a> = (SearchQueue<Grid,'a>.empty).Insert initialEl
    let e: Set<Grid> = Set.empty
    let rec loop (e: Set<Grid>) (f: SearchQueue<Grid,'a>) = 
        if f.IsEmpty
        then None
        else 
            let n, f' = f.Pop
            if p.GoalTest n.state
            then 
                Some (retrieveSolution n)
            else 
                let e' = e.Add n.state
                let f'' = p.Actions n.state |> List.fold (fun (f'': SearchQueue<Grid,'a>) (a,s) ->
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

let rec orderGoals' (grid: Grid) (prevH: Map<Pos * Pos, int>) (isMA: bool) (boxTypeToId: Map<ObjType,Set<Guid>>) (agentColorToId: Map<Color,Set<AgentIdx>>) (orderedGoals : List<Pos * Goal>) (unsolvedGoals : Set<Pos * Goal>) = 
    // set other unsolvedGoals to walls
    // search for box of Goal type
    // return true if exists
    let isSolvableGoal ((goalPos, gt): Pos * Goal) = 
        let grid' = 
          grid
          |> flip (Set.fold (fun g (pos, _) -> g.AddWall pos)) (unsolvedGoals.Remove (goalPos, gt))
        match graphSearch (BoxPointerProblem (grid', goalPos, gt, false, prevH, boxTypeToId.[gt] |> Set.map (fun id -> grid'.boxPos.[id]))) with
        | Some n -> 
          if isMA |> not then true else
            let boxPos = n.Head.state.searchPoint.Value
            let boxColor =
              match n.Head.state.dynamicGrid.[boxPos] with
              | Box(_,_,c) -> Some c
              | _ -> None
            match graphSearch (AgentPointerProblem (grid', boxPos, boxColor.Value, false, prevH, agentColorToId.[boxColor.Value] |> Set.map (fun id -> grid'.agentPos.[id]))) with
            | Some _ -> true
            | None -> false  
        | None -> false  
    
    if unsolvedGoals.IsEmpty
        then orderedGoals
        else 
          match Set.toList unsolvedGoals |> List.tryFind isSolvableGoal with
          | Some g -> 
            orderGoals' grid prevH isMA boxTypeToId agentColorToId (g :: orderedGoals) (unsolvedGoals.Remove g)
          | None -> 
            (Set.toList unsolvedGoals) @ orderedGoals

let orderGoals grid prevH isMA boxTypeToId agentColorToId unsolvedGoals = 
  orderGoals' grid prevH isMA boxTypeToId agentColorToId [] unsolvedGoals

type BFSSokobanProblem(agentIdx: AgentIdx, grid: Grid, goalTest) = 
    inherit ISokobanProblem<ActionMeta>()
    let numAgents = grid.agentPos.Count
    let agentInt = agentIdx |> System.Char.GetNumericValue |> int
    
    override p.Initial = grid
    override p.Actions s = 
        // Array.create agentCount NOP
        Grid.validSokobanActions agentIdx s
        |> List.map (fun ((a, d), g) -> 
            let action' = Array.create numAgents NOP
            Array.set action' agentInt a
            ((action', d), g))
    override p.GoalTest s = goalTest s
    override p.ChildNode n a s = 
      let child = gridToNode' n a s
      {child with value = child.cost}
    override p.initialAction () = Array.create numAgents NOP, Set.empty

// AStarSokobanProblem(boxGuid: Guid, agentIdx: AgentIdx, grid: Grid, prevHValues: Map<Pos*Pos, int>, goalTest)
type AStarSokobanProblem (boxGuid: Guid, agentIdx: AgentIdx, grid: Grid, prevHValues: Map<Pos*Pos, int>, goalTest, heuristicTransformer) =
    inherit ISokobanProblem<ActionMeta>()

    let numAgents = grid.agentPos.Count
    let agentInt = agentIdx |> System.Char.GetNumericValue |> int
    override p.Initial = grid
    override p.Actions s =
        Grid.validSokobanActions agentIdx s
        |> List.map (fun ((a, d), g) -> 
            let action' = Array.create numAgents NOP
            Array.set action' agentInt a
            ((action', d), g))

    override p.GoalTest s = goalTest agentIdx s
                            
    override p.ChildNode n a s = 
        let boxPos = Map.find boxGuid s.boxPos
        let agentPos = Map.find agentIdx s.agentPos
        let agentH = Map.find (boxPos,agentPos) prevHValues
        let node = gridToNode' n a s
        let h = 8*agentH
        {node with value = node.cost + heuristicTransformer boxPos h }
        //{node with value = node.cost + manhattanDistance goalPos boxPos + manhattanDistance boxPos agentPos}
    override p.initialAction () = Array.create numAgents NOP, Set.empty

    /// Heuristic search with goal.
    new ((goalPos,nextGoalPos): Pos * (Pos option), boxGuid: Guid, agentIdx: AgentIdx, grid: Grid, prevHValues: Map<Pos*Pos, int>) = 
        let goalTest agentIdx s = 
            let boxPos = Map.find boxGuid s.boxPos
            let agentPos = Map.find agentIdx s.agentPos
            match (goalPos = boxPos) with
            | false -> false
            | true ->
              match Option.isSome nextGoalPos with
              | false -> true
              | true ->
                let hasPath = 
                  PosPointerProblem(grid.AddWall(goalPos), agentPos, nextGoalPos.Value, prevHValues)
                  |> graphSearch
                  |> Option.isSome
                if hasPath then true else
                  PosPointerProblem(grid, agentPos, nextGoalPos.Value, prevHValues)
                  |> graphSearch
                  |> Option.isNone
        let heuristicTransformer boxPos h = 
            let boxH = Map.find (goalPos,boxPos) prevHValues
            h + 10*boxH
        AStarSokobanProblem (boxGuid, agentIdx, grid, prevHValues, goalTest, heuristicTransformer)

    /// Heuristic search without goal.
    new (boxGuid: Guid, agentIdx: AgentIdx, grid: Grid, prevHValues: Map<Pos*Pos, int>, goalTest) =
        AStarSokobanProblem (boxGuid, agentIdx, grid, prevHValues, goalTest, (fun _ h -> h))
