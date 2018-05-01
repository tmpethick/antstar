// Learn more about F# at http://fsharp.org
module Program
open System
open System.IO
open Domain
open Grid
open Search

let rec toOutput (l: Action list): string list =
    match l with
    | []                -> []
    | NOP::t            -> "[NoOp]"::toOutput t
    | Move (_,d)::t     -> "[Move("+string d+")]"::toOutput t
    | Pull(_,ad,bd)::t  -> "[Pull("+string ad+","+string bd+")]"::toOutput t
    | Push(_,ad,bd)::t  -> "[Push("+string ad+","+string bd+")]"::toOutput t
    | MovePointer(_)::t -> toOutput t
    
let rec printOutput (l:string list) = 
    match l with
    | []   -> []
    | h::t -> printfn "%s" h :: printOutput t

let printPossibleOutcomes state = Grid.allValidActions state 
                                  |> List.map (fun (_, s') -> printfn "%O" s') 
                                  |> ignore

let rec testActions state = function
    | action :: actions ->
        match apply action state with
        | Error err      -> failwith (err.ToString ())
        | Success state' -> printfn "%O" state'
                            testActions state' actions
    | [] -> printfn "Is Goal state: %O" (allGoalsMet state)

//let testMinimal () = 
//    let problem = SokobanProblem ("./levels/SAminimal.lvl", Search.allGoalsMet)
//    let actions = [Move ('0', E); Move ('0', E); Push ('0', E, E)]
//    testActions problem.Initial actions
let getGrid filename =
  let lines = Path.Combine(__SOURCE_DIRECTORY__, filename) |> readLines
  let colors, gridLines = parseColors Map.empty (lines)
  parseMap colors (gridLines |> addIdx)

let getGoals grid =
  grid.staticGrid
  |> Map.map (fun _ v ->
    match v with
    | Goal c -> Some c
    | SEmpty -> None)
  |> Map.filter (fun _ x -> Option.isSome x)
  |> Map.map (fun _ x -> Option.get x)
  |> Map.toArray


let testSimpleSearch (level: string) () = 
    let grid = getGrid level

    getGoals grid
    |> Array.map (fun (p,c) ->
      let problem = SokobanProblem (grid,p,c)
      match graphSearch problem with
      | None -> failwith "SASsimple1 solution not found"
      | Some x -> 
        x
        |> List.map (fun n -> n.action)
        |> toOutput
        |> printOutput
    )

let findAdjecent (predicate: DynamicObject -> bool) (pos: Pos) (grid: Grid) : (Pos * DynamicObject) option = 
    [N;S;E;W]
    |> List.map (flip posFromDir pos)
    |> List.map (fun pos -> pos, grid.dynamicGrid.TryFind pos)
    |> List.tryFind (function
                     | (pos, Some o) when predicate o -> true
                     | _ -> false)
    |> Option.map (function | (p, Some o) -> (p, o) | _ -> failwith "expected DynamicObject to be Some")

let getAgentPos (g: Grid) = (Seq.head g.agentPos).Value

let isBoxOfType    t = function | Box (_,t', _)   -> t' = t | _ -> false
let isAgentOfColor c = function | Agent (_, c') -> c' = c | _ -> false

let isBox = function | Box _ -> false | _ -> true

// Search closest box (remove agents. agent at goal. goal state is agent next to box)
// Argument: Agent is at box position implies that agent must have been adjecent in previous state.
//let pickBox ((goalPos, objtype): Pos * Goal) (grid: Grid): Context<Pos * Box> = 
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

//// Search closest agent (remove all boxes. remove all invalid agents. place agent at pos)
//let pickAgent ((boxPos, boxColor): Pos * Color) (grid: Grid): Context<Pos * Agent> = 
//    let agent = defaultAgent
//    let grid' = grid
//                |> Grid.filterDynamicObjects (fun _ -> not << isBox)
//                |> Grid.filterAgents (snd >> (=)boxColor)
//                |> Grid.addAgent boxPos agent
//    let isGoal g = 
//            let agentPos = getAgentPos g
//            Option.isSome (findAdjecent (isAgentOfColor boxColor) agentPos g)
//    match graphSearch (SokobanProblem (grid')) with
//    | None -> Error NoBoxToAgentPathFound
//    | Some solution -> 
//        let endState = (List.head solution).state
//        let agentPos = getAgentPos endState
//        match findAdjecent (isAgentOfColor boxColor) agentPos endState with
//        | Some (pos, Agent a) -> Success (pos, a)
//        | _ -> failwith "If a solution is found an agent MUST be adjecent. Otherwise you're in dead trouble."

//let solveSubGoal (grid: Grid) (posGoal: Pos * Goal) : Action list = 
//    let (Success (boxPos, box)) = pickBox posGoal grid
//    let (Success (agentPos, agent)) = pickAgent (boxPos, snd box) grid
//    // TODO: search agent -> box -> goal
//    // Set other boxes as walls. Remove all other goals
//    // Search using manhatten distance
//    []

//let testSolveSubGoals () = 
//    let problem = SokobanProblem ("./levels/SAsimple1.lvl", Search.allGoalsMet)
//    // find all goals (TODO: order somehow)
//    let state = problem.Initial
//    let goals = state.GetGoals ()
//    List.collect (solveSubGoal state) goals |> ignore

type CommandLineOptions = {
    level: string;
    }

let rec parseCommandLineInput args options = 
    match args with
    | [] -> 
        options  
    | "-lvl"::h::t -> 
        let updatedOptions = { options with level= (Path.GetFullPath h) }
        parseCommandLineInput t updatedOptions
    | h::t -> 
        eprintfn "Option '%s' is unrecognized" h
        parseCommandLineInput t options 

[<EntryPoint>]
let main args =
    //testMinimal ()
    let mutable options = {
        level = Path.Combine(__SOURCE_DIRECTORY__, "./levels/testlevels/SAtest2.lvl")
        }
    options <- parseCommandLineInput (Array.toList args) options

    testSimpleSearch options.level ()

    //let problem = SokobanProblem ("./levels/SAsimple1.lvl", Search.allGoalsMet)
    //printfn "%O" problem.Initial

    //match graphSearch problem with
    //| Some solution -> solution 
    //                 |> Seq.map (fun n -> n.state) 
    //                 |> Seq.iter (printf "%O")
    //| None -> printf "No solution found"

    // testSolveSubGoals ()

    // printfn "Press any key to exit"
    // Console.ReadKey() |> ignore
    0 // return an integer exit code
