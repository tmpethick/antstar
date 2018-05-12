﻿// Learn more about F# at http://fsharp.org
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

let getGridFromLines lines = 
  let colors, gridLines = parseColors Map.empty (lines)
  eprintfn "%O" colors
  parseMap colors (gridLines |> addIdx)

let getGrid filename =
  let lines = Path.Combine(__SOURCE_DIRECTORY__, filename) |> readLines
  getGridFromLines lines

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

// goal ordering (wait)
// solution path
// filter obstacles
// remove from path by recursive search

let pickBox ((goalPos, gt): Pos * Goal) (grid: Grid): (Box * Pos) * Pos list = 
    // let grid' = grid
            //    |> Grid.removeAgents
            //    |> Grid.filterDynamicObjects (fun _ -> isOfTypeIfBox gt)
    match graphSearch (BoxPointerProblem (grid, goalPos, gt)) with
        | Some s -> 
            let state = s.Head.state
            let boxPos = state.searchPoint.Value
            match Map.find boxPos state.dynamicGrid with
            | Box box -> (box, boxPos), List.map (fun n -> n.state.searchPoint.Value) s
            | _ -> failwith "search should always lead to a box..." 
        | None -> failwith "NoGoalToBoxPathFound"

let pickAgent ((boxPos, boxColor): Pos * Color) (grid: Grid): (Agent * Pos) * Pos list = 
    // let grid' = grid
            //    |> Grid.filterDynamicObjects (fun _ -> not << isBox)
            //    |> Grid.filterAgents (snd >> (=)boxColor)
    match graphSearch (AgentPointerProblem (grid, boxPos, boxColor)) with
        | Some s -> 
            let state = s.Head.state
            let agentPos = state.searchPoint.Value
            match Map.find agentPos state.dynamicGrid with
            | Agent agent -> (agent, agentPos), List.map (fun n -> n.state.searchPoint.Value) s
            | _ -> failwith "search should always lead to a box..." 
        | None -> failwith "NoGoalToBoxPathFound"

// Its an obstacle if the agent cannot remove it itself
let getObstacleFromPath (agentColor: Color) (state: Grid) (path: Pos list): Pos option = 
    path 
    |> List.tryFind (fun pos -> 
        match Map.find pos state.dynamicGrid with 
        | Box box -> agentColor <> getBoxColor box
        | Agent _ -> true
        | _ -> false)

// pos of all DEmpty pos
let freePos (state: Grid): Set<Pos> =
    Map.toSeq state.dynamicGrid
    |> Seq.filter (fun (pos, t) -> match t with | DEmpty -> true | _ -> false)
    |> Seq.map fst
    |> Set.ofSeq

let getActionsAndResultingState (solution: Node<Grid,Action> list) = 
    (List.map (fun n -> n.action) solution), solution.Head.state

let solveObstacle (pos: Pos) (state: Grid) = 
    let goalPositions = freePos state
    let goalTest agentIdx s = let agentPos = Map.find agentIdx s.agentPos
                              goalPositions.Contains agentPos
    match Map.find pos state.dynamicGrid with
    // | Box _ -> 
    // TODO: solve goal with obstacles..
    | Agent a -> BFSSokobanProblem (getAgentIdx a, state, goalTest) 
                 |> graphSearch
                 |> Option.get
                 |> getActionsAndResultingState
    | _ -> failwith "Pos should contain Box or Agent obstacle"

// boxPos and agentPos might change during the recursion.. which is not intended
let createClearPath (goalPos, goal) grid = 
    eprintfn "createClearPath: %O\n" (goalPos, goal, grid)
    let (box, boxPos), goalPath = pickBox (goalPos, goal) grid
    let (agent, agentPos), boxPath = pickAgent (boxPos, getBoxColor box) grid
    let solutionPath = boxPath @ goalPath

    let rec clearPath gridAcc solutionAcc = 
        match getObstacleFromPath (getAgentColor agent) gridAcc solutionPath with
        | Some obstacle -> 
            let obsActionSolution, gridAcc' = solveObstacle obstacle gridAcc
            clearPath gridAcc' (solutionAcc @ obsActionSolution)
        | None -> 
            gridAcc, solutionAcc
    let grid', actions = clearPath grid []
    eprintfn "%O" grid'
    box, agent, (grid', actions)

let solveGoal (goalPos, goal) grid : Action list * Grid = 
        eprintfn "Solving goal %O" goal
        let box, agent, (grid', actions) = createClearPath (goalPos, goal) grid

        match AStarSokobanProblem (goalPos, getId box, getAgentIdx agent, grid') |> graphSearch with
        | Some solution -> 
            let state = solution.Head.state.AddWall goalPos
            actions @ (List.map (fun n -> n.action) solution), state
        | None -> failwith "come on"

let rec solveGoals actions grid = function 
    | [] -> actions
    | goal :: goals -> let actions', grid' = solveGoal goal grid
                       solveGoals (actions @ List.rev actions') grid' goals 

let testGoalOrdering grid = 
    let goals = orderGoals grid (Set.ofList (getGoals grid))
    solveGoals [] grid goals |> toOutput |> printOutput

let isOfTypeIfBox gt d = (((not << isBox) d) || (isBoxOfType gt d))

let rec readInput() =
    seq {
        let o = Console.ReadLine()
        yield o
        if o <> "" then yield! readInput()
    }

[<EntryPoint>]
let main args =
    // let lines = readInput ()
    let lines = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__,"levels/testlevels/","MAAgentObstacle.lvl"))
    let grid = getGridFromLines (Seq.toList lines)
    testGoalOrdering grid |> ignore
    0
