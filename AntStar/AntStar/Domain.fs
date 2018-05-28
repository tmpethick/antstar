module Domain
open System
// Blue is default
type Color      =  Blue | Red | Green | Cyan | Magenta | Orange | Pink | Yellow
type ObjType    = char
type Box        = Guid * ObjType * Color
type Goal       = ObjType
type AgentIdx   = char
type Agent      = AgentIdx * Color
type StaticObject  = Goal of Goal | SEmpty
type DynamicObject = Agent of Agent | Box of Box | Wall | DEmpty

let getId ((id,_,_): Box) = id
let getType ((_,t,_): Box) = t
let getBoxColor ((_,_,color): Box) = color
let isBoxOfType t = function | Box (_,t', _) -> t' = t | _ -> false

let getAgentIdx (idx,_) = idx
let getAgentColor (_,color) = color

let defaultColor = Blue
let defaultAgent: Agent = ('0', defaultColor)

type Pos = int * int
type LockedPos = Set<Pos>
type HistoryLockedPos = Set<Pos>
type StaticGrid  = Map<Pos, StaticObject>
type DynamicGrid = Map<Pos, DynamicObject>
type Desire =
  | FindBox of Pos * Goal
  | FindAgent of Pos * ObjType * Color
  | MoveAgent of Pos * AgentIdx * (Pos list)
  | MoveBox of Pos * Guid * (Pos list)
  | IsGoal
type Dir = N | E | S | W
type Action = 
  | NOP 
  | Move of AgentIdx * Dir 
  | Push of AgentIdx * Dir * Dir 
  | Pull of AgentIdx * Dir * Dir
  | MovePointer of Dir
type ActionMeta = Action [] * LockedPos
type ActionList = ActionMeta list

type Errors =
  // Action errors
  | AgentNotOnMap
  | PositionOccupied
  | ColorMismatch
  | NotAssociatedObject
  // Grid errors
  | BoxPositionIsNotBox
  | AgentPositionIsNotAgent
  | InvalidGridPosition
  | OutOfBounds
  | SearchPointerIsNone
  // Search errors
  | NoGoalToBoxPathFound
  | NoBoxToAgentPathFound
  // Default errors
  | BooleanConditionFalse
type Context<'a> =
  | Success of 'a
  | Error of Errors


let getDesireCost = function
  | FindBox(_) -> 10
  | FindAgent(_) -> 2
  | MoveAgent(_) -> 1
  | MoveBox(_) -> 20
  | IsGoal -> 0

  
/// Use argument and pass it along
let (|>>) x g = g x; x
/// Option binder
let (?>>) m f = Option.bind f m
/// Option mapper
let (?|>) m f = Option.map f m
/// Option checker
let (?>>?) m c = Option.bind (fun x -> match c x with | true -> Some x | false -> None) m
/// Option default argument
let (?|) = defaultArg

/// Context binder
let (!>>) m f = 
  match m with
  | Success x -> f x
  | Error x -> Error x
/// Context mapper
let (!|>) m f = 
  match m with
  | Success x -> Success (f x)
  | Error x -> Error x
/// Context checker
let (!>>?) (m: Context<'a>) (c: 'a -> bool) : Context<'a> =
  (!>>) m (fun x -> match c x with | true -> Success x | false -> Error BooleanConditionFalse)
/// Context default argument
let (!|) m arg =
  match m with
  | Success x -> x
  | Error _ -> arg

let unWrap = function
    | Success s -> s
    | Error _ -> failwith "Tried to unwrap an error state"
let flip f a b = f b a

// String concatenation helpers
let flatten xs = xs |> List.fold (+) ""
let toLine xs = (flatten xs) + "\n"

// List helpers
let cartesian xs ys = List.collect (fun x -> List.map (fun y -> x, y) ys) xs
let addIdx (arr : 'a list) = List.mapi (fun i line -> (i, line)) arr

type Cost = int

[<CustomComparison; CustomEquality>]
type Node<'s,'a> = {
    state  : 's;
    parent : Node<'s,'a> option;
    action : 'a;
    cost   : Cost;
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
    override x.GetHashCode() = x.value

type ColoredString = {text: String; background: ConsoleColor; color: ConsoleColor;}
type ColoredLines = ColoredString [] []

let toConsoleColor = function 
  | Blue    -> ConsoleColor.Blue
  | Red     -> ConsoleColor.Red
  | Green   -> ConsoleColor.Green
  | Cyan    -> ConsoleColor.Cyan
  | Magenta -> ConsoleColor.Magenta
  | Orange  -> ConsoleColor.DarkMagenta
  | Pink    -> ConsoleColor.DarkYellow
  | Yellow  -> ConsoleColor.Yellow