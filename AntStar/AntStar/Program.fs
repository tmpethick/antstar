﻿// Learn more about F# at http://fsharp.org
module Program
open System
open System.IO
open Domain
open Grid
open Search

let printPossibleOutcomes state = Grid.allValidActions '0' state 
                                  |> List.map (fun (_, s') -> printfn "%O" s') 
                                  |> ignore

let rec testActions state = function
    | action :: actions ->
        match apply action state with
        | Error err      -> failwith (err.ToString ())
        | Success state' -> printfn "%O" state'
                            testActions state' actions
    | [] -> printfn "Is Goal state: %O" (allGoalsMet state)

let testMinimal () = 
    let problem = SokobanProblem ("./levels/SAminimal.lvl", Search.allGoalsMet)
    let actions = [Move ('0', E); Move ('0', E); Push ('0', E, E)]
    testActions problem.Initial actions

let testSimpleSearch () = 
    let problem = SokobanProblem ("./levels/SAsimple1.lvl", Search.allGoalsMet)
    match graphSearch problem with
    | None -> failwith "SASsimple1 solution not found"
    | Some _ -> ()

let findAdjecent (predicate: DynamicObject -> bool) (pos: Pos) (grid: Grid) : (Pos * DynamicObject) option = 
    [N;S;E;W]
    |> List.map (flip posFromDir pos)
    |> List.map (fun pos -> pos, grid.dynamicGrid.TryFind pos)
    |> List.tryFind (function
                     | (pos, Some o) when predicate o -> true
                     | _ -> false)
    |> Option.map (function | (p, Some o) -> (p, o) | _ -> failwith "expected DynamicObject to be Some")

let getAgentPos (g: Grid) = (Seq.head g.agentPos).Value

let isBoxOfType    t = function | Box (t', _)   -> t' = t | _ -> false
let isAgentOfColor c = function | Agent (_, c') -> c' = c | _ -> false

let isBox = function | Box _ -> false | _ -> true

// Search closest box (remove agents. agent at goal. goal state is agent next to box)
// Argument: Agent is at box position implies that agent must have been adjecent in previous state.
let pickBox ((goalPos, objtype): Pos * Goal) (grid: Grid): Context<Pos * Box> = 
    let agent = defaultAgent
    let grid' = grid
                |> Grid.removeAgents
                |> Grid.addAgent goalPos agent
    let isGoal g = 
            let agentPos = getAgentPos g
            Option.isSome (findAdjecent (isBoxOfType objtype) agentPos g)
    match graphSearch (SokobanProblem (grid', fst agent, isGoal)) with
    | None -> Error NoGoalToBoxPathFound
    | Some solution -> 
        let endState = (List.head solution).state
        let agentPos = getAgentPos endState
        match findAdjecent (isBoxOfType objtype) agentPos endState with
        | Some (pos, Box b) -> Success (pos, b)
        | _ -> failwith "If a solution is found a box MUST be adjecent. Otherwise you're in dead trouble."

// Search closest agent (remove all boxes. remove all invalid agents. place agent at pos)
let pickAgent ((boxPos, boxColor): Pos * Color) (grid: Grid): Context<Pos * Agent> = 
    let agent = defaultAgent
    let grid' = grid
                |> Grid.filterDynamicObjects (fun _ -> not << isBox)
                |> Grid.filterAgents (snd >> (=)boxColor)
                |> Grid.addAgent boxPos agent
    let isGoal g = 
            let agentPos = getAgentPos g
            Option.isSome (findAdjecent (isAgentOfColor boxColor) agentPos g)
    match graphSearch (SokobanProblem (grid', fst agent, isGoal)) with
    | None -> Error NoBoxToAgentPathFound
    | Some solution -> 
        let endState = (List.head solution).state
        let agentPos = getAgentPos endState
        match findAdjecent (isAgentOfColor boxColor) agentPos endState with
        | Some (pos, Agent a) -> Success (pos, a)
        | _ -> failwith "If a solution is found an agent MUST be adjecent. Otherwise you're in dead trouble."

let solveSubGoal (grid: Grid) (posGoal: Pos * Goal) : Action list = 
    let (Success (boxPos, box)) = pickBox posGoal grid
    let (Success (agentPos, agent)) = pickAgent (boxPos, snd box) grid
    // TODO: search agent -> box -> goal
    // Set other boxes as walls. Remove all other goals
    // Search using manhatten distance
    []

let testSolveSubGoals () = 
    let problem = SokobanProblem ("./levels/SAsimple1.lvl", Search.allGoalsMet)
    // find all goals (TODO: order somehow)
    let state = problem.Initial
    let goals = state.GetGoals ()
    List.collect (solveSubGoal state) goals |> ignore

[<EntryPoint>]
let main argv =
    testMinimal ()
    testSimpleSearch ()

    let problem = SokobanProblem ("./levels/SAsimple1.lvl", Search.allGoalsMet)
    printfn "%O" problem.Initial

    match graphSearch problem with
    | Some solution -> solution 
                     |> Seq.map (fun n -> n.state) 
                     |> Seq.iter (printf "%O")
    | None -> printf "No solution found"

    // testSolveSubGoals ()

    printfn "Press any key to exit"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
