open System
open System.IO
open System.Text.RegularExpressions

// Blue is default
type Color      =  Blue | Red | Green | Cyan | Magenta | Orange | Pink | Yellow
type ObjType    = char
type Box        = ObjType * Color
type Goal       = ObjType
type AgentIdx   = char
type Agent      = AgentIdx * Color
type StaticObject  = Goal of Goal | Wall | SEmpty
type DynamicObject = Agent of Agent | Box of Box | DEmpty

type Pos = int * int
type StaticGrid  = Map<Pos, StaticObject>
type DynamicGrid = Map<Pos, DynamicObject>

// String concatenation helpers
let flatten xs = xs |> List.fold (+) ""
let toLine xs = (flatten xs) + "\n"

// List helpers
let cartesian xs ys = List.collect (fun x -> List.map (fun y -> x, y) ys) xs
let addIdx (arr : 'a list) = List.mapi (fun i line -> (i, line)) arr

// State data structure
type Grid     = { 
    staticGrid  : StaticGrid; 
    dynamicGrid : DynamicGrid;
    agentPos    : Map<AgentIdx, Pos>
    width  : int;
    height : int } with 
        override g.ToString() = 
            List.map (fun j -> 
                List.map (fun i -> match Map.find (i,j) g.staticGrid with
                                    | Goal t           -> t.ToString().ToLower()
                                    | Wall             -> "+"
                                    | SEmpty           -> match Map.find (i,j) g.dynamicGrid with
                                                            | Agent (t, _) -> t.ToString()
                                                            | Box (t, _)   -> t.ToString()
                                                            | DEmpty       -> " "
                    ) [0..g.width-1] |> toLine
                ) [0..g.height-1] |> flatten
        member g.SetAgent corr agent = { g with 
                                            dynamicGrid = g.dynamicGrid |>Map.add corr (Agent agent)
                                            agentPos    = g.agentPos    |>Map.add (fst agent) corr }
        member g.MoveAgent agentIdx pos = let oldPos  = g.agentPos       |> Map.find agentIdx
                                          let agent   = g.dynamicGrid |> Map.find oldPos 
                                          let dynGrid = g.dynamicGrid 
                                                           |> Map.add oldPos DEmpty
                                                           |> Map.add pos agent
                                          let agentPos = g.agentPos |> Map.add agentIdx pos
                                          { g with dynamicGrid = dynGrid; agentPos = agentPos }

let emptyGrid w h = 
    let coords = cartesian [0..w-1] [0..h-1]
    {staticGrid  = new StaticGrid  (Seq.map (fun c -> c, SEmpty) coords);
     dynamicGrid = new DynamicGrid (Seq.map (fun c -> c, DEmpty) coords);
     agentPos    = Map.empty;
     width       = w;
     height      = h}

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
    | []              -> (colorMap, [])
    | (line :: lines : string list) -> 
        match line.Split [|':'|] with
         | [|c;objList|]  -> 
            let colorMap' = Array.fold (fun s (o: string) -> Map.add (o.Chars(0)) (parseColor c) s) colorMap (objList.Split [|','|])
            parseColors colorMap' lines
         | _     -> (colorMap, line :: lines)

let getColor k colorMap = 
    match Map.tryFind k colorMap with
        | Some c -> c
        | None   -> Blue

let matchRegex pattern c = Regex.Match(new string [|c|], pattern).Success

let parseMap colorMap (lines: list<int * string>) : Grid = 
    let w = List.max (List.map (snd >> String.length) lines)
    let h = List.length lines
    List.fold (fun grid l ->
        let j = fst l
        List.fold (fun grid -> function
            | (i, c) when matchRegex @"[0-9]" c -> grid.SetAgent (i,j) (c, getColor c colorMap)
            | (i, c) when matchRegex @"[A-Z]" c -> let box = Box (c, getColor c colorMap)
                                                   { grid with dynamicGrid = grid.dynamicGrid |> Map.add (i,j) box }
            | (i, c) when matchRegex @"[a-z]" c -> { grid with staticGrid = grid.staticGrid |> Map.add (i,j) (Goal c) }
            | (i, '+')                          -> { grid with staticGrid = grid.staticGrid |> Map.add (i,j) Wall }
            | (_, ' ')                          -> grid
            | _                                 -> failwith "not a valid map character"
        ) grid (snd l |> Seq.toList |> addIdx)
    ) (emptyGrid w h) lines

// Example use
let lines = readLines "./levels/MAsimple4.lvl"
let colors, gridLines = parseColors Map.empty (lines)
let grid = parseMap colors (gridLines |> addIdx)
printfn "%O" grid

