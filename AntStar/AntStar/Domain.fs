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

let getAgentIdx (idx,_) = idx
let getAgentColor (_,color) = color

let defaultColor = Blue
let defaultAgent: Agent = ('0', defaultColor)

type Pos = int * int
type LockedPos = Set<Pos>
type HistoryLockedPos = Set<Pos>
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
  | OutOfBounds
  | SearchPointerIsNone
  // Search errors
  | NoGoalToBoxPathFound
  | NoBoxToAgentPathFound
  
type Context<'a> =
  | Success of 'a
  | Error of Errors

type Desire =
  | FindBox of Pos * Goal
  | FindAgent of Pos * ObjType * Color
  | MoveAgent of Pos * AgentIdx * (Pos list)
  | MoveBox of Pos * Guid * (Pos list)
  | IsGoal

let getDesireCost = function
  | FindBox(_) -> 10
  | FindAgent(_) -> 2
  | MoveAgent(_) -> 1
  | MoveBox(_) -> 20
  | IsGoal -> 0

type Dir = N | E | S | W
type Action = 
  | NOP 
  | Move of AgentIdx * Dir 
  | Push of AgentIdx * Dir * Dir 
  | Pull of AgentIdx * Dir * Dir
  | MovePointer of Dir

let (|?????>) m f = match m with Some x -> f x | None -> None
let (|?>) m f = match m with Success x ->  f x | Error e -> Error e 
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
    override x.GetHashCode() = x.value // TODO: 

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

// From https://blogs.msdn.microsoft.com/chrsmith/2008/10/01/f-zen-colored-printf/
let cprintf c bg fmt = 
    Printf.kprintf
        (fun s ->
            let oldBg = System.Console.BackgroundColor
            let oldColor = System.Console.ForegroundColor
            try
              System.Console.ForegroundColor <- c;
              System.Console.BackgroundColor <- bg;
              System.Console.Error.Write s
            finally
              System.Console.ForegroundColor <- oldColor
              System.Console.BackgroundColor <- oldBg)
        fmt

let cprintLines (coloredLines: ColoredLines): unit =
  coloredLines 
  // |> ignore
  |> Array.iter (fun lines -> 
      lines |> Array.iter (fun line -> cprintf line.color line.background "%O" line.text)
      eprintfn "")
