// Learn more about F# at http://fsharp.org
module Program
open System
open System.IO
open Domain
open Grid
open Search
open System.Dynamic

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
  |> Map.toList

let unWrap = function
    | Success s -> s
    | Error _ -> failwith "nooooo"


let pickBox ((goalPos, gt): Pos * Goal) (grid: Grid): Context<Pos * Box> = 
    let grid' = grid
            //    |> Grid.removeAgents
            //    |> Grid.filterDynamicObjects (fun _ -> isOfTypeIfBox gt)
    match graphSearch (BoxPointerProblem (grid', goalPos, gt)) with
        | Some s -> 
            let state = s.Head.state
            let boxPos = state.searchPoint.Value
            match Map.find boxPos state.dynamicGrid with
            | Box box -> Success (boxPos, box)
            | _ -> failwith "search should always lead to a box..." 
        | None -> Error NoGoalToBoxPathFound

let pickAgent ((boxPos, boxColor): Pos * Color) (grid: Grid): Context<Pos * Agent> = 
    let grid' = grid
            //    |> Grid.filterDynamicObjects (fun _ -> not << isBox)
            //    |> Grid.filterAgents (snd >> (=)boxColor)
    match graphSearch (AgentPointerProblem (grid', boxPos, boxColor)) with
        | Some s -> 
            let state = s.Head.state
            let agentPos = state.searchPoint.Value
            match Map.find agentPos state.dynamicGrid with
            | Agent agent -> Success (agentPos, agent)
            | _ -> failwith "search should always lead to a box..." 
        | None -> Error NoGoalToBoxPathFound

let solveGoal (goalPos, goal) grid : Action list * Grid = 
        let boxPos, box = pickBox (goalPos, goal) grid |> unWrap
        let agentPos, agent = pickAgent (boxPos, getBoxColor box) grid |> unWrap
        match AStarSokobanProblem (goalPos, getId box, getAgentIdx agent, grid) |> graphSearch with
        | Some solution -> 
            let state = solution.Head.state.AddWall goalPos
            solution |> List.map (fun n -> n.action), state
        | None -> failwith "come on"

let rec solveGoals actions grid = function 
    | [] -> actions
    | goal :: goals -> let actions', grid' = solveGoal goal grid
                       solveGoals (actions @ List.rev actions') grid' goals 

let testGoalOrdering level = 
    let grid = getGrid level
    let goals = orderGoals grid (Set.ofList (getGoals grid))
    solveGoals [] grid goals |> printfn "%A"

let isOfTypeIfBox gt d = (((not << isBox) d) || (isBoxOfType gt d))

let testSA level = 
    let grid = getGrid level
    let goals = orderGoals grid (Set.ofList (getGoals grid))
    printfn "%A" goals

let testSimpleSearch (level: string) () = 
    let grid = getGrid level

    getGoals grid
    |> List.map (fun (p,c) ->
      let problem = SokobanProblem (grid,p,c)
      match graphSearch problem with
      | None -> failwith "SASsimple1 solution not found"
      | Some x -> 
        x
        |> List.map (fun n -> n.action)
        |> toOutput
        |> printOutput
    )

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
    runTests: bool;
    }

let rec parseCommandLineInput args options = 
    match args with
    | [] -> 
        options  
    | "-lvl"::h::t -> 
        let updatedOptions = { options with level= (Path.GetFullPath h); runTests = true }
        parseCommandLineInput t updatedOptions
    | h::t -> 
        eprintfn "Option '%s' is unrecognized" h
        parseCommandLineInput t options 

let runTests () = 
    testGoalOrdering "./levels/SAanagram.lvl" |> ignore

[<EntryPoint>]
let main args =
    //testMinimal ()
    let mutable options = {
        level = Path.Combine(__SOURCE_DIRECTORY__, "./levels/testlevels/SAtest2.lvl")
        runTests = true
        }
    options <- parseCommandLineInput (Array.toList args) options

    if options.runTests 
        then runTests ()
        else testSimpleSearch options.level () |> ignore

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
