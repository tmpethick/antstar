module Domain

// Blue is default
type Color      =  Blue | Red | Green | Cyan | Magenta | Orange | Pink | Yellow
type ObjType    = char
type Box        = ObjType * Color
type Goal       = ObjType
type AgentIdx   = char
type Agent      = AgentIdx * Color
type StaticObject  = Goal of Goal | SEmpty
type DynamicObject = Agent of Agent | Box of Box | Wall | DEmpty

type Pos = int * int
type StaticGrid  = Map<Pos, StaticObject>
type DynamicGrid = Map<Pos, DynamicObject>

type Errors =
  // Action errors
  | AgentNotOnMap
  | PositionOccupied
  | ColorMismatch
  | NotAssociatedObject
  // Grid errors
  | BoxPositionIsNotBox
  | InvalidGridPosition
  
type Context<'a> =
  | Success of 'a
  | Error of Errors

type Dir = N | E | S | W
type Action = 
  | NOP 
  | Move of AgentIdx * Dir 
  | Push of AgentIdx * Dir * Dir 
  | Pull of AgentIdx * Dir * Dir

let (|?????>) m f = match m with Some x -> f x | None -> None
let (|?>) m f = match m with Success x ->  f x | Error e -> Error e 
let flip f a b = f b a



// String concatenation helpers
let flatten xs = xs |> List.fold (+) ""
let toLine xs = (flatten xs) + "\n"

// List helpers
let cartesian xs ys = List.collect (fun x -> List.map (fun y -> x, y) ys) xs
let addIdx (arr : 'a list) = List.mapi (fun i line -> (i, line)) arr
