module Search

open System
open System.IO
open FSharpx.Collections
open Grid 
open Domain 

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

type SokobanProblem (grid, goalPos, goal) = 
    inherit Problem<Grid,Action>()

    override p.Initial = {grid with desires = (FindBox(goalPos,goal) :: grid.desires); searchPoint = Some goalPos}
    override p.Actions s = Grid.allValidActions s
    override p.GoalTest s = s.desires.Head = IsGoal
    override p.ChildNode n a s = Grid.getChild n a s
    override p.initialAction () = NOP

    new(filename, goalPos, goal) = 
        let lines = filename |> readLines
        let colors, gridLines = parseColors Map.empty (lines)
        let grid = parseMap colors (gridLines |> addIdx)
        SokobanProblem (grid, goalPos, goal)

let allGoalsMet (grid: Grid) = 
    grid.staticGrid
     |> Map.forall (fun gpos e -> 
        match e with 
        | SEmpty -> true
        | Goal gt -> 
            match grid.dynamicGrid.TryFind gpos with
            | Some (Box (bt, _)) -> gt = bt
            | _ -> false)


let rec retrieveSolution (n: Node<'s,'a>) = 
    match n.parent with
    | None    -> []
    | Some n' -> n :: retrieveSolution n'

let graphSearch (p: SokobanProblem) = 
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

