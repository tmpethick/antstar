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
   member this.CostP (cost: Cost) (_: 's) (_: 'a) (_: 's) = cost + 1
   member this.Heuristic (_: Node<'s,'a>) : Cost = 0
   member this.ValueP (_: 's) : Cost = 0

let root (p: Problem<'s,'a>): Node<'s,'a> = 
        let s = p.Initial
        { state  = s;
          parent = None;
          action = p.initialAction ();
          cost   = 0;
          depth  = 0;
          value  = p.ValueP s}

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

[<AbstractClass>]
type ISokobanProblem () =
   inherit Problem<Grid,Action>()

type SokobanProblem (grid, goalPos, goal) = 
    inherit ISokobanProblem()

    override p.Initial = {grid with desires = (FindBox(goalPos,goal) :: grid.desires); searchPoint = Some goalPos}
    override p.Actions s = Grid.allValidActions s
    override p.GoalTest s = s.desires.Head = IsGoal
    override p.ChildNode n a s = 
        let child = Grid.getChild n a s
        printfn "%O" child.state 
        child
    override p.initialAction () = NOP

    new(grid, goalPos, goal) = SokobanProblem (grid, goalPos, goal)

type PointerProblem (grid, goalPos, goal) = 
    inherit ISokobanProblem()
    override p.Initial = {grid with desires = [(FindBox(goalPos,goal))]; searchPoint = Some goalPos }
    override p.Actions s = Grid.validMovePointer s
    override p.GoalTest s = match s.searchPoint with 
                            | Some pointer -> Option.exists (isBoxOfType goal) (s.dynamicGrid.TryFind pointer)
                            | None -> false
    override p.ChildNode n a s = Grid.getChild n a s
    override p.initialAction () = NOP

    new(grid, goalPos, goal) = PointerProblem (grid, goalPos, goal)

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
    let pathCost = 0
    let f = (SearchQueue<Grid,Action>.empty).Insert initialEl
    let e: Set<Grid> = Set.empty
    let rec loop (f: SearchQueue<Grid,Action>) = 
        let frontierStates = Map.map (fun s n -> n.state) f.m
        // printfn "%O" frontierStates
        if f.IsEmpty
        then None
        else 
            let n, f' = f.Pop
            if p.GoalTest n.state
            then Some (retrieveSolution n)
            else 
                let e' = e.Add n.state
                let f'' = p.Actions n.state |> List.fold (fun (f'':SearchQueue<Grid,Action>) (a,s) ->
                    let c = p.ChildNode n a s
                    let isNew = not ((e'.Contains c.state) || (f''.Contains c.state))
                    let isCheaper = 
                        match f''.TryFind c.state with
                        | Some n -> n.value > c.value
                        | None -> false
                    if isNew || isCheaper 
                    then f''.Insert c
                    else f'') f'
                loop f''
    loop f

// TODO: multiple goals can use same box. Might lead ordering with non-trivial solution.
let rec orderGoals' (grid: Grid) (orderedGoals : List<Pos * Goal>) (unsolvedGoals : Set<Pos * Goal>) = 
    // set other unsolvedGoals to walls
    // search for box of Goal type
    // return true if exists
    let isSolvableGoal ((goalPos, gt): Pos * Goal) = 
        let grid' = grid
                   |> Grid.removeAgents
                   |> Grid.filterDynamicObjects (fun _ d -> (((not << isBox) d) || (isBoxOfType gt d)))
                   |> flip (Set.fold (fun g (pos, _) -> g.AddWall pos)) unsolvedGoals
        printfn "%O" grid'
        match graphSearch (PointerProblem (grid', goalPos, gt)) with
        | Some _ -> true
        | None -> false  

    if unsolvedGoals.IsEmpty 
        then orderedGoals
        else match Set.toList unsolvedGoals |> List.tryFind isSolvableGoal with
             | Some g -> orderGoals' grid (g :: orderedGoals) (unsolvedGoals.Remove g)
             | None -> failwith (sprintf "%A are unsolvable" unsolvedGoals)

let orderGoals grid unsolvedGoals = orderGoals' grid [] unsolvedGoals

