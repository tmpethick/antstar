module Grid
open Domain
open System.IO
open System.Text.RegularExpressions

type Grid     = { 
    staticGrid  : StaticGrid; 
    dynamicGrid : DynamicGrid;
    agentPos    : Map<AgentIdx, Pos>
    width  : int;
    height : int } with 
        override g.ToString() = 
          [0..g.height-1]
          |> List.map (fun j -> 
              [0..g.width-1]
              |> List.map (fun i -> 
                  match Map.find (i,j) g.staticGrid with
                  | Goal t           -> t.ToString().ToLower()
                  | SEmpty           -> 
                    match Map.find (i,j) g.dynamicGrid with
                    | Agent (t, _) -> t.ToString()
                    | Wall         -> "+"
                    | Box (t, _)   -> t.ToString()
                    | DEmpty       -> " "
                    ) 
              |> toLine) 
          |> flatten
        member g.SetAgent corr agent = 
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
          | Some (Box(t,c)) ->
            let grid' =
              g.dynamicGrid
              |> Map.add pTo (Box(t,c))
              |> Map.add pFrom DEmpty
            Success {g with dynamicGrid = grid'}
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


let emptyGrid w h = 
    let coords = cartesian [0..w-1] [0..h-1]
    {staticGrid  = new StaticGrid  (Seq.map (fun c -> c, SEmpty) coords);
     dynamicGrid = new DynamicGrid (Seq.map (fun c -> c, DEmpty) coords);
     agentPos    = Map.empty;
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

let apply (action: Action) (grid: Grid) : Context<Grid> =
  match action with
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
        match grid.dynamicGrid.[newAPos], grid.dynamicGrid.[newBPos] with
        | Box(_,c),DEmpty when c=c' ->
          grid.MoveBox newAPos newBPos
          |?> fun grid' -> Success (grid'.MoveAgent a newAPos)
        | Box(_,_),DEmpty -> Error ColorMismatch
        | Box(_,_),_ -> Error PositionOccupied
        | _,_ -> Error NotAssociatedObject
  | Pull(a,ad,bd) ->
    match Map.tryFind a grid.agentPos with
    | None   -> Error AgentNotOnMap
    | Some curAPos -> 
      let newAPos = posFromDir ad curAPos
      let curBPos = posFromDir (flipDir bd) curAPos
      grid.GetAgent a
      |?> fun (_,c') -> 
        match grid.dynamicGrid.[newAPos], grid.dynamicGrid.[curBPos] with
        | DEmpty, Box(_,c) when c=c' ->
          grid.MoveAgent a newAPos
          |> fun grid' -> (grid'.MoveBox curBPos curAPos)
        | DEmpty, Box(_,_) -> Error ColorMismatch
        | _,Box(_,_) -> Error PositionOccupied
        | _,_ -> Error NotAssociatedObject

let allValidActions (a: AgentIdx) (grid: Grid) =
  let pushPull = 
    [N;E;S;W] 
    |> List.pairwise
    |> List.collect (fun (d1,d2) -> [Push(a,d1,d2);Pull(a,d1,d2)])
  
  [|N;E;S;W|]
  |> Array.fold (fun cur d -> Move(a,d) :: cur) pushPull
  |> fun x -> NOP :: x
  |> List.toArray
  |> Array.Parallel.map (fun a -> a,apply a grid)
  |> Array.Parallel.map (fun (a,c) -> match c with Success s -> Some(a,s) | Error _ -> None)
  |> Array.filter Option.isSome
  |> Array.Parallel.map Option.get

  
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
              grid.SetAgent (i,j) (c, getColor c colorMap)

            | (i, c) when matchRegex @"[A-Z]" c -> 
              let box = Box (c, getColor c colorMap)
              { grid with dynamicGrid = grid.dynamicGrid |> Map.add (i,j) box }
            
            | (i, '+')                          -> 
              { grid with dynamicGrid = grid.dynamicGrid |> Map.add (i,j) Wall }

            | (i, c) when matchRegex @"[a-z]" c -> 
              { grid with staticGrid = grid.staticGrid |> Map.add (i,j) (Goal c) }

            | (_, ' ')                          -> grid
            | _                                 -> failwith "not a valid map character"
        ) grid
    ) (emptyGrid w h)