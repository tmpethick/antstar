module Grid
open System.IO
open System.Text.RegularExpressions
open System
open Domain
open System.Collections.Generic

[<Interface>]
type IGrid =
  abstract width : int with get
  abstract height : int with get
  abstract staticGrid  : StaticGrid with get
  abstract dynamicGrid : DynamicGrid with get
  abstract searchPoint : Pos option with get

type Grid     = { 
  staticGrid  : StaticGrid 
  dynamicGrid : DynamicGrid
  agentPos    : Map<AgentIdx, Pos>
  boxPos      : Map<Guid, Pos>
  searchPoint : Pos option
  width  : int
  height : int } with 
    static member GridToStringTransformer (f: Grid -> Pos -> String) (g: Grid): String = 
      [0..g.height-1]
      |> List.map (fun j -> 
        [0..g.width-1]
        |> List.map (fun i -> f g (i,j)) 
        |> toLine)
      |> flatten

    static member PosToString (g: Grid) ((i,j): Pos): String * ConsoleColor = 
      if Option.exists ((=) (i,j)) g.searchPoint
      then "O", ConsoleColor.Black
      else
        match Map.find (i,j) g.dynamicGrid with
        | Agent (t, c) -> t.ToString(), toConsoleColor c
        | Wall         -> "+", ConsoleColor.Black
        | Box (_,t, c)-> t.ToString().ToUpper(), toConsoleColor c
        | DEmpty       -> 
          match Map.find (i,j) g.staticGrid with
          | Goal t -> t.ToString().ToLower(), ConsoleColor.Black
          | SEmpty -> " ", ConsoleColor.Black

    override g.ToString() = Grid.GridToStringTransformer (fun g -> Grid.PosToString g >> fst) g
    member g.AddWall corr = 
      {g with 
        dynamicGrid = 
          g.dynamicGrid 
          |> Map.add corr Wall }
    member g.AddBox corr (box: Box) = 
      {g with 
        dynamicGrid = 
          g.dynamicGrid 
          |> Map.add corr (Box box); 
        boxPos = 
          g.boxPos
          |> Map.add (getId box) corr}
    member g.AddAgent corr agent = 
      {g with 
        dynamicGrid = 
          g.dynamicGrid 
          |> Map.add corr (Agent agent)
        agentPos = 
          g.agentPos    
          |> Map.add (fst agent) corr }
    member g.MoveAgent agentIdx pos = 
      let oldPos = g.agentPos |> Map.find agentIdx
      match g.dynamicGrid |> Map.tryFind oldPos with
      | Some (Agent(x)) ->
        Success 
          {g with 
            dynamicGrid = 
              g.dynamicGrid 
              |> Map.add oldPos DEmpty
              |> Map.add pos (Agent(x)); 
            agentPos = 
              g.agentPos 
              |> Map.add agentIdx pos }
      | Some (_) -> Error AgentPositionIsNotAgent
      | None -> Error InvalidGridPosition
    member g.MoveBox pFrom pTo =
      match Map.tryFind pFrom g.dynamicGrid with
      | Some (Box(id,t,c)) ->
        let grid' =
          g.dynamicGrid
          |> Map.add pTo (Box(id,t,c))
          |> Map.add pFrom DEmpty
        Success {g with dynamicGrid = grid'; boxPos = Map.add id pTo g.boxPos}
      | Some (_) -> Error BoxPositionIsNotBox
      | None -> Error InvalidGridPosition
        
    member g.GetAgent agentIdx =
      g.agentPos
      |> Map.tryFind agentIdx
      ?>> flip Map.tryFind g.dynamicGrid
      |> fun x ->
        match x with
        | Some (Agent(t,c)) -> Success (t,c)
        | Some (_)
        | None -> Error AgentNotOnMap
    member g.GetUnsolvedGoals () = 
      g.GetGoals ()
      |> Set.filter (fun (pos, gt) -> 
        match Map.tryFind pos g.dynamicGrid with
        | Some o -> not (isBoxOfType gt o)
        | None -> true)

    member g.GetGoals () = 
      g.staticGrid
      |> Map.toArray
      |> Array.Parallel.map (fun (pos, obj) ->
        match obj with
        | Goal gt -> Some (pos, gt)
        | SEmpty -> None)
      |> Array.filter Option.isSome
      |> Array.map Option.get
      |> Set.ofArray
    member inline g.RemoveAgents () = 
        let removeAgent _ = function | Agent _ -> DEmpty | a -> a
        {g with
          agentPos = Map.empty; 
          dynamicGrid = 
            g.dynamicGrid 
            |> Map.map removeAgent}
    member inline g.FilterAgents (predicate: Agent -> bool) =
      {g with 
        dynamicGrid = 
          g.dynamicGrid 
          |> Map.filter (fun _ -> function 
            | Agent a -> predicate a 
            |_ -> true); 
        agentPos = 
          g.agentPos 
          |> (Map.filter (fun _ pos -> 
            match g.dynamicGrid.TryFind pos with
            | Some (Agent agent) -> predicate agent
        
            | _ -> true)) }

    // TODO: update box position
    member inline g.FilterDynamicObjects mapper = 
      {g with 
        dynamicGrid = 
          g.dynamicGrid 
          |> Map.map (fun pos (d: DynamicObject) -> 
            if mapper pos d then d else DEmpty) }

    static member inline filterDynamicObjects mapper (g: Grid) = g.FilterDynamicObjects mapper
    static member inline filterAgents predicate (g: Grid) = g.FilterAgents predicate
    static member inline addAgent pos agent (g:Grid) = g.AddAgent pos agent
    static member inline removeAgents (g: Grid) = g.RemoveAgents ()

let emptyGrid w h = 
  let coords = cartesian [0..w-1] [0..h-1]
  {staticGrid  = new StaticGrid  (Seq.map (fun c -> c, SEmpty) coords);
    dynamicGrid = new DynamicGrid (Seq.map (fun c -> c, DEmpty) coords);
    agentPos    = Map.empty;
    boxPos      = Map.empty;
    searchPoint = None;
    width       = w;
    height      = h}

let posFromDir (d: Dir) ((x,y): Pos) =
  let (x',y') =
    match d with
    | N -> (0,-1)
    | E -> (1,0)
    | S -> (0,1)
    | W -> (-1,0)
  (x+x',y+y')

let moveAgent a pos (grid: Grid) = grid.MoveAgent a pos
let moveBox fromPos toPos (grid: Grid) = grid.MoveBox fromPos toPos

let apply (action: Domain.Action) (grid: Grid) : Context<Grid> =
  match action with
  | NOP -> Success grid
  | MovePointer(d) ->
    match grid.searchPoint with
    | None -> Error SearchPointerIsNone
    | Some p ->
      let newPos = posFromDir d p 
      match grid.dynamicGrid.TryFind newPos with
      | None -> Error InvalidGridPosition
      | Some Wall -> Error PositionOccupied
      | _ -> Success {grid with searchPoint = Some newPos}

  | Move(a,d) -> 
    match Map.tryFind a grid.agentPos with
    | None -> Error AgentNotOnMap
    | Some p -> 
      let newPos = posFromDir d p
      match grid.dynamicGrid.[newPos] with
      | DEmpty -> grid.MoveAgent a newPos
      | _      -> Error PositionOccupied

  | Push(a,ad,bd) ->
    match Map.tryFind a grid.agentPos with
    | None -> Error AgentNotOnMap
    | Some curAPos -> 
      let newAPos = posFromDir ad curAPos
      let newBPos = posFromDir bd newAPos
      let moveBoxAndAgent = fun (_,c') -> 
        match grid.dynamicGrid.TryFind newAPos, grid.dynamicGrid.TryFind newBPos with
        | Some (Box(_,_,c)),Some DEmpty when c=c' -> 
           (!>>) (moveBox newAPos newBPos grid) (moveAgent a newAPos)
        | Some (Box(_)),Some DEmpty           -> Error ColorMismatch
        | Some (Box(_)),_                     -> Error PositionOccupied
        | _,_                                 -> Error NotAssociatedObject
      (!>>) (grid.GetAgent a) moveBoxAndAgent

  | Pull(a,ad,bd) ->
    match Map.tryFind a grid.agentPos with
    | None   -> Error AgentNotOnMap
    | Some curAPos -> 
      let newAPos = posFromDir ad curAPos
      let curBPos = posFromDir bd curAPos
      let moveAgentAndBox = (fun (_,c') -> 
        match grid.dynamicGrid.TryFind newAPos, grid.dynamicGrid.TryFind curBPos with
        | None, _ |  _, None                     -> Error OutOfBounds 
        | Some DEmpty, Some (Box(_,_,c)) when c=c' ->
          (!>>) (moveAgent a newAPos grid) (moveBox curBPos curAPos)
        | Some DEmpty, Some (Box(_))           -> Error ColorMismatch
        | _, Some (Box(_))                     -> Error PositionOccupied
        | _,_                                  -> Error NotAssociatedObject)
      (!>>) (grid.GetAgent a) moveAgentAndBox

let filterValidActions grid f actions = 
  actions
  |> List.toArray
  |> Array.Parallel.map (fun a -> 
    match apply (f a) grid with 
    | Success s -> Some(a,s) 
    | Error _   -> None)
  |> Array.filter Option.isSome
  |> Array.Parallel.map Option.get
  |> Array.toList

let movePointer = 
  [N;E;S;W]
  |> List.fold (fun cur d -> MovePointer(d) :: cur) []

let validMovePointer (grid: Grid) = 
  movePointer |> filterValidActions grid id

let allGridActions (agentIdx: AgentIdx) (grid: Grid) = 
  let agentPos = Map.find agentIdx grid.agentPos
  let nopDep = Set.ofList [agentPos]
  
  let beforeMove = Set.singleton agentPos
  let moves = 
    [N;S;E;W]
    |> List.map (fun d -> 
      let afterMove = posFromDir d agentPos
      let move = beforeMove |> Set.add afterMove
      ((Move (agentIdx, d), move)))

  cartesian [N;S;E;W] [N;S;E;W]
  |> List.fold (fun cur (d1,d2) ->
    let p1 = posFromDir d1 agentPos
    let p2 = posFromDir d2 agentPos
    let p12 = posFromDir d2 p1
    let pullBefore = Set.ofList [agentPos; p2]
    let pullAfter = Set.ofList [p1; p12]
    let pushBefore = Set.ofList [agentPos; p1]
    let pushAfter = pullBefore
    let pull = Set.union pullBefore pullAfter
    let push = Set.union pushBefore pushAfter
    (Push(agentIdx, d1, d2), pull)
    :: (Pull(agentIdx,d1,d2), push)
    :: cur
    ) ((NOP, nopDep) :: moves)


let validGridActions agentIdx (grid: Grid) = 
  allGridActions agentIdx grid 
  |> filterValidActions grid fst

let readLines filename =
  filename
  |> File.ReadAllLines
  |> Array.toList

let parseColor = function
  | "blue"    -> Blue 
  | "red"     -> Red 
  | "green"   -> Green 
  | "cyan"    -> Cyan 
  | "magenta" -> Magenta 
  | "orange"  -> Orange 
  | "pink"    -> Pink 
  | "yellow"  -> Yellow 
  | _         -> failwith "Not a valid color"

let rec parseColors colorMap = function
  | []                            -> (colorMap, [])
  | (line :: lines : string list) -> 
    match line.Split [|':'|] with
    | [|c;objList|]  -> 
      let colorMap' = 
        (objList.Split [|','|])
        |> Array.Parallel.map (fun s -> s.Trim ())
        |> Array.fold (fun s (o: string) -> 
          Map.add (o.Chars(0)) (parseColor c) s
        ) colorMap

      parseColors colorMap' lines
    | _              -> (colorMap, line :: lines)

let getColor k colorMap = 
  match Map.tryFind k colorMap with
  | Some c -> c
  | None   -> Blue

let matchRegex pattern c = Regex.Match(new string [|c|], pattern).Success

let parseMap colorMap (lines: list<int * string>) : Grid = 
  let w = List.max (List.map (snd >> String.length) lines)
  let h = List.length lines
  lines
  |> List.fold (fun grid l ->
    let j = fst l
    (snd l |> Seq.toList |> addIdx)
    |> List.fold (fun grid -> function
      | (i, c) when matchRegex @"[0-9]" c -> 
        grid.AddAgent (i,j) (c, getColor c colorMap)

      | (i, c) when matchRegex @"[A-Z]" c -> 
        let id = Guid.NewGuid()
        let box: Box = (id,Char.ToLower c, getColor c colorMap)
        grid.AddBox (i,j) box
            
      | (i, '+')                          -> 
        { grid with dynamicGrid = grid.dynamicGrid |> Map.add (i,j) Wall }

      | (i, c) when matchRegex @"[a-z]" c -> 
        { grid with staticGrid = grid.staticGrid |> Map.add (i,j) (Goal (Char.ToLower c)) }

      | (_, ' ')                          -> grid
      | _                                 -> failwith "not a valid map character"
    ) grid
  ) (emptyGrid w h)

// No heurstics
let gridToNode (n: Node<Grid,Action []>) a s =
  let cost = n.cost + 1
  { n with state = s; cost = cost; value = cost; action = a; parent = Some n; }

let gridToNode' (n: Node<Grid, Action [] * Set<Pos>>) a s =
  let cost = n.cost + 1
  { n with state = s; cost = cost; value = cost; action = a; parent = Some n; }

let getChild (n: Node<Grid,Action []>) (appliedAction: Action []) (newState: Grid) : Node<Grid,Action []> = 
  gridToNode n appliedAction newState


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
        validMovePointer {grid with searchPoint = Some p} 
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