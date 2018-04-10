open FSharpx.Collections

type Cost = int

[<CustomComparison; CustomEquality>]
type Node<'s,'a> = {
    state  : 's;
    parent : Node<'s,'a> option;
    action : 'a;
    cost   : Cost;
    depth  : int;
    value  : int;}
    with
    interface System.IComparable with
      member x.CompareTo y = 
        match y with
        | :? Node<'s,'a> as y' -> x.value - y'.value
        | _             -> invalidArg "y" "Not a QueueElement"
    override x.Equals(yobj) =  
       match yobj with 
       | :? Node<'s,'a> as y -> x.value = y.value
       | _ -> false
    override x.GetHashCode() = x.value // TODO: 

[<AbstractClass>]
type Problem<'s, 'a> () =
   abstract member Initial: unit -> 's
   abstract member Actions: 's -> list<'a * 's>
   abstract member GoalTest: 's -> bool
   abstract member ChildNode: Node<'s,'a> -> 'a -> Node<'s,'a>
   abstract member initialAction: unit -> 'a
   member this.CostP (cost: Cost) (_: 's) (_: 'a) (_: 's) = cost + 1
   member this.Heuristic (_: Node<'s,'a>) : Cost = 0
   member this.ValueP (_: 's) : Cost = 0

let root (p: Problem<'s,'a>): Node<'s,'a> = 
        let s = p.Initial ()
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

let foldFrontier<'s,'a when 's: comparison> (p: Problem<'s,'a>) n (e': Set<'s>) (f'':SearchQueue<'s,'a>) (a,_) =
    let c = p.ChildNode n a
    let hasNotSeenBefore = not ((e'.Contains c.state) || (f''.Contains c.state))
    let isCheaper = 
        match f''.TryFind c.state with
        | Some n -> n.value > c.value
        | None -> false
    if hasNotSeenBefore || isCheaper 
    then f''.Insert c
    else f''

let graphSearch<'s,'a when 's: comparison> (p: Problem<'s,'a>) : 's option = 
    let initialEl = root p
    let pathCost = 0
    let f = (SearchQueue<'s,'a>.empty).Insert initialEl
    let e: Set<'s> = Set.empty
    let rec loop (f: SearchQueue<'s,'a>) = 
        if f.IsEmpty 
        then None
        else 
            let n, f' = f.Pop
            if p.GoalTest n.state
            then Some n.state // solution
            else 
                let e' = e.Add n.state
                let f'' = p.Actions n.state |> List.fold (foldFrontier p n e') f'
                loop f''
    loop f
