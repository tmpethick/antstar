module Grid
open System.IO
open System.Text.RegularExpressions
open System
open Domain




type Grid     = { 
    staticGrid  : StaticGrid 
    dynamicGrid : DynamicGrid
    agentPos    : Map<AgentIdx, Pos>
    boxPos      : Map<Guid, Pos>
    desires     : List<Desire>
    searchPoint : Pos option
    width  : int
    height : int } with 
        override g.ToString() = 
          [0..g.height-1]
          |> List.map (fun j -> 
              [0..g.width-1]
              |> List.map (fun i -> 
                  if Option.exists ((=) (i,j)) g.searchPoint
                  then "O"
                  else
                    match Map.find (i,j) g.dynamicGrid with
                    | Agent (t, _) -> t.ToString()
                    | Wall         -> "+"
                    | Box (id,t, _)-> t.ToString().ToUpper()
                    | DEmpty       -> 
                      match Map.find (i,j) g.staticGrid with
                      | Goal t -> t.ToString().ToLower()
                      | SEmpty -> " ") 
              |> toLine) 
          |> flatten
        
        member g.AddWall corr = 
          { g with dynamicGrid = g.dynamicGrid |> Map.add corr Wall }
        member g.AddBox corr (box: Box) = 
          { g with dynamicGrid = g.dynamicGrid |> Map.add corr (Box box); boxPos = Map.add (getId box) corr g.boxPos }
        member g.AddAgent corr agent = 
          { g with 
              dynamicGrid = g.dynamicGrid |> Map.add corr (Agent agent)
              agentPos    = g.agentPos    |> Map.add (fst agent) corr }
        member g.MoveAgent agentIdx pos = 
          let oldPos  = g.agentPos    |> Map.find agentIdx
          let agent   = g.dynamicGrid |> Map.find oldPos 
          let dynGrid = g.dynamicGrid 
                        |> Map.add oldPos DEmpty
                        |> Map.add pos agent
          let agentPos = g.agentPos   |> Map.add agentIdx pos
          { g with dynamicGrid = dynGrid; agentPos = agentPos }
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
          |?????> flip Map.tryFind g.dynamicGrid
          |> fun x ->
            match x with
            | Some (Agent(t,c)) -> Success (t,c)
            | Some (_)
            | None -> Error AgentNotOnMap
        member g.GetGoals () = 
            g.staticGrid
            |> Map.toList
            |> List.map (fun (pos, obj) ->
                match obj with
                | Goal gt -> Some (pos, gt)
                | SEmpty -> None)
            |> List.filter Option.isSome
            |> List.map Option.get
        member inline g.RemoveAgents () = 
            let removeAgent pos = function | Agent _ -> DEmpty | a -> a
            {g with
                agentPos = Map.empty; 
                dynamicGrid = g.dynamicGrid |> Map.map removeAgent}
        member inline g.FilterAgents (predicate: Agent -> bool) =
            { g with dynamicGrid = g.dynamicGrid 
                                   |> Map.filter (fun _ -> function 
                                       | Agent a -> predicate a 
                                       |_ -> true); 
                     agentPos = g.agentPos 
                                |> (Map.filter (fun _ pos -> 
                                    match g.dynamicGrid.TryFind pos with
                                    | Some (Agent agent) -> predicate agent
        
                                    | _ -> true)) }

        // TODO: update box position
        member inline g.FilterDynamicObjects mapper = 
          {g with dynamicGrid = g.dynamicGrid |> Map.map (fun pos (d: DynamicObject) -> if mapper pos d then d else DEmpty) }

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
     desires = [];
     searchPoint = None;
     width       = w;
     height      = h}

let flipDir = function
  | N -> S
  | E -> W
  | S -> N
  | W -> E

let posFromDir (d: Dir) ((x,y): Pos) =
  let (x',y') =
    match d with
    | N -> (0,-1)
    | E -> (1,0)
    | S -> (0,1)
    | W -> (-1,0)
  (x+x',y+y')

let apply (action: Domain.Action) (grid: Grid) : Context<Grid> =
  match action with
  | MovePointer(d) ->
    match grid.searchPoint with
    | None -> Error SearchPointerIsNone
    | Some p ->
      let newPos = posFromDir d p 
      match grid.dynamicGrid.TryFind newPos with
      | None -> Error InvalidGridPosition
      | Some Wall  -> Error PositionOccupied
      | _ -> Success {grid with searchPoint = Some newPos}
  | NOP       -> Success grid
  | Move(a,d) -> 
    match Map.tryFind a grid.agentPos with
    | None   -> Error AgentNotOnMap
    | Some p -> 
      let newPos = posFromDir d p
      match grid.dynamicGrid.[newPos] with
      | DEmpty -> Success (grid.MoveAgent a newPos)
      | _      -> Error PositionOccupied

  | Push(a,ad,bd) ->
    match Map.tryFind a grid.agentPos with
    | None   -> Error AgentNotOnMap
    | Some curAPos -> 
      let newAPos = posFromDir ad curAPos
      let newBPos = posFromDir bd newAPos
      grid.GetAgent a
      |?> fun (_,c') -> 
        match grid.dynamicGrid.TryFind newAPos, grid.dynamicGrid.TryFind newBPos with
        | Some (Box(_,_,c)),Some DEmpty when c=c' -> 
            grid.MoveBox newAPos newBPos
           |?> fun grid' -> Success (grid'.MoveAgent a newAPos)
        | Some (Box(_,_,_)),Some DEmpty           -> Error ColorMismatch
        | Some (Box(_,_,_)),_                     -> Error PositionOccupied
        | _,_                                   -> Error NotAssociatedObject
  | Pull(a,ad,bd) ->
    match Map.tryFind a grid.agentPos with
    | None   -> Error AgentNotOnMap
    | Some curAPos -> 
      let newAPos = posFromDir ad curAPos
      let curBPos = posFromDir bd curAPos
      grid.GetAgent a
      |?> fun (_,c') -> 
        match grid.dynamicGrid.TryFind newAPos, grid.dynamicGrid.TryFind curBPos with
        | None, _ |  _, None                     -> Error OutOfBounds 
        | Some DEmpty, Some (Box(_,_,c)) when c=c' ->
          grid.MoveAgent a newAPos
          |> fun grid' -> (grid'.MoveBox curBPos curAPos)
        | Some DEmpty, Some (Box(_,_,_))           -> Error ColorMismatch
        | _, Some (Box(_,_,_))                     -> Error PositionOccupied
        | _,_                                    -> Error NotAssociatedObject


let filterValidActions grid actions = 
  actions
  |> List.toArray
  |> Array.map (fun a -> a,apply a grid)
  |> Array.map (fun (a,c) -> 
        match c with 
        | Success s -> Some(a,s) 
        | Error _   -> None)
  |> Array.filter Option.isSome
  |> Array.map Option.get
  |> Array.toList

let movePointer = 
  [N;E;S;W]
  |> List.fold (fun cur d -> MovePointer(d) :: cur) []

let validMovePointer (grid: Grid) = 
  movePointer |> filterValidActions grid

let allSokobanActions (agentIdx: AgentIdx) (grid: Grid) = 
  let moves = 
    [N;S;E;W]
    |> List.map (fun d -> Move (agentIdx, d))
  
  cartesian [N;S;E;W] [N;S;E;W]
  |> List.fold (fun cur (d1,d2) ->
    Push(agentIdx, d1, d2) :: Pull(agentIdx,d1,d2) :: cur
    ) (NOP :: moves)

let validSokobanActions agentIdx (grid: Grid) = 
  allSokobanActions agentIdx grid |> filterValidActions grid

// TODO: parallelise
let allValidActions (grid: Grid) =
  let desireActions =
    match grid.desires.Head with
    | IsGoal 
    | FindAgent _ 
    | FindBox _ -> []
    | MoveAgent(_,aId,_) -> 
      [N;E;S;W] 
      |> List.map (fun d -> Move(aId,d))
    | MoveBox(bPos,id,_) ->
      let validAgents = 
        grid.agentPos
        |> Map.toArray
        |> Array.map (fun (aid,p) -> grid.GetAgent aid, p)
        |> Array.map (fun (context,p) -> 
          match context with
          | Success(aid,color) -> aid,color,p
          | Error _ -> failwith "uncaught custom error")
      
      let boxPos =
        match Map.tryFind id grid.boxPos with
        | Some bp -> bp
        | None -> failwith "unknown box"
      
      let agentsNextToBox =
        validAgents
        |> Array.map (fun (aid,ac,apos) ->
          let isNextToBox =
            [N;E;S;W]
            |> List.map (fun d -> d,posFromDir d apos)
            |> List.exists (fun (_,p) -> p = boxPos)
          aid,isNextToBox)
        |> Array.filter (fun (_,isNextToBox) -> isNextToBox)
        |> Array.map fst
      
      let moveAgent =
        validAgents
        |> Array.toList
        |> List.fold (fun cur (aid,_,_) ->
          [N;E;S;W]
          |> List.fold (fun cur' d -> Move(aid,d) :: cur') cur
        ) []
      
      agentsNextToBox
      |> Array.toList
      |> List.fold (fun cur aid ->
        cartesian [N;E;S;W] [N;E;S;W]
        |> List.fold (fun cur' (d1,d2) -> Push(aid,d1,d2) :: Pull(aid,d1,d2) :: cur') cur
      ) moveAgent
       
  desireActions
  |> List.fold (fun cur a -> a :: cur) movePointer
  |> filterValidActions grid

// Parse map
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
              |> Array.map (fun s -> s.Trim ())
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

//let isGoal (grid: Grid) =
//  match grid.desires.Head, grid.searchPoint with
//  | (FindAgent(_,(objType,c)),Some p) ->
//    match grid.dynamicGrid.TryFind p with
//    | None -> false
//    | Some (Agent(aId, c')) -> 
//      c = c'
//  | _ -> false

let rec getObstructions curDesire (n: Node<Grid,Action>) =
  match n.parent with
  | Some n' ->
    if curDesire <> n'.state.desires.Head then None else
    match n.state.searchPoint with
    | Some p -> 
      match n.state.dynamicGrid.TryFind p with
      | Some DEmpty -> getObstructions curDesire n'
      | Some x -> Some (p,x)
      | None -> None // return error here
    | None -> None // return error here
  | None -> None
  
let rec getPositions curDesire positions (n: Node<Grid,Action>) =
  if curDesire <> n.state.desires.Head then positions else
  match n.parent with
  | Some n' ->
    match n.state.searchPoint with
    | Some p -> getPositions curDesire (p :: positions) n'
    | None -> positions // return error
  | None -> positions
 
 
let tryGetObstructionState curDesire n s = 
  match getObstructions curDesire n with
  | Some (objP,obj) -> 
    let forbiddenPositions = getPositions curDesire [] n
    let desire = 
      match obj with
      | Agent((idx,_)) -> MoveAgent(objP, idx, forbiddenPositions)
      | Box(id,objType',c') -> MoveBox(objP, id, forbiddenPositions)
      | _ -> failwith "Unknown obstruction"
    Some 
      {s with 
        desires = (desire :: s.desires)
        searchPoint = Some objP }
  | None -> None

let applyFindBoxDesire (n: Node<Grid,Action>) (s: Grid) (p: Pos) (goal: Goal) = 
  match s.dynamicGrid.TryFind p with
  | Some (Box(_,objType,c)) when objType = goal ->
    match tryGetObstructionState s.desires.Head n s with
    | Some obsState -> obsState
    | None -> {s with desires = FindAgent(p, objType, c) :: s.desires}
  | _ -> s

let applyFindAgentDesire (n: Node<Grid,Action>) (s: Grid) (p: Pos) (boxType: ObjType) (c: Color) = 
  match s.dynamicGrid.TryFind p with
  | Some (Agent(aId,c')) when c = c' ->
    match tryGetObstructionState s.desires.Head n s with
    | Some obsState -> obsState
    | None -> {s with desires = IsGoal :: s.desires}
  | _ -> s

let applyMoveAgentDesire (n: Node<Grid,Action>) (s: Grid) (aid: AgentIdx) (forbidden: Pos list) = 
  match s.agentPos.TryFind aid with
  | Some p -> 
    match forbidden |> List.contains p with
    | true -> s
    | false -> {s with desires = s.desires.Tail}
  | None -> failwith "Unknown agent"

let applyMoveBoxDesire (n: Node<Grid,Action>) (s: Grid) (bId: Guid) (forbidden: Pos list) = 
  let boxPos =
    match Map.tryFind bId s.boxPos with
    | Some bp -> bp
    | None -> failwith "unknown box"

  match forbidden |> List.contains boxPos with
  | true -> s
  | false -> {s with desires = s.desires.Tail}

// No heurstics
let gridToNode (n: Node<Grid,Action>) a s = 
  let cost = n.cost + 1
  { n with state = s; cost = cost; value = cost; action = a; parent = Some n; }

let getChild (n: Node<Grid,Action>) (appliedAction: Action) (newState: Grid) : Node<Grid,Action> = 
  let resultState =
    match newState.searchPoint, newState.desires.Head with
    | None, _ -> newState
    | Some p, FindBox(goalPos,goal)-> applyFindBoxDesire n newState p goal
    | Some p, FindAgent(boxPos,boxType,c) -> applyFindAgentDesire n newState p boxType c
    | Some p, MoveAgent(aPos,aid,forbidden)-> applyMoveAgentDesire n newState aid forbidden
    | Some p, MoveBox(boxPos, bId, forbidden)-> applyMoveBoxDesire n newState bId forbidden
    | Some p, IsGoal-> newState

  gridToNode n appliedAction resultState
