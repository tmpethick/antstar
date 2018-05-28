module Problem
open Domain
open Search
open System
open Grid


let pickBox ((goalPos, gt): Pos * Goal) (prevH: Map<Pos*Pos,int>) (boxTypeToId: Map<ObjType, Set<Guid>>) (grid: Grid) : (Box * Pos) * Pos list = 
    match graphSearch (BoxPointerProblem (grid, goalPos, gt, true, prevH, boxTypeToId.[gt] |> Set.map (fun id -> grid.boxPos.[id]))) with
    | Some [] ->
      match Map.find goalPos grid.dynamicGrid with
      | Box box -> (box, goalPos), []
      | _ -> failwith "search should always lead to a box..." 
    | Some s -> 
        let state = s.Head.state
        let boxPos = state.searchPoint.Value
        match Map.find boxPos state.dynamicGrid with
        | Box box -> (box, boxPos), List.append (List.map (fun n -> n.state.searchPoint.Value) s) [goalPos]
        | _ -> failwith "search should always lead to a box..." 
    | None -> failwith "NoGoalToBoxPathFound"

let pickAgent ((boxPos, boxColor): Pos * Color) (prevH: Map<Pos*Pos,int>) (agentColorToId: Map<Color,Set<AgentIdx>>) (grid: Grid): (Agent * Pos) * Pos list =
    match graphSearch (AgentPointerProblem (grid, boxPos, boxColor, true, prevH, agentColorToId.[boxColor] |> Set.map (fun id -> grid.agentPos.[id]))) with
    | Some s -> 
        let state = s.Head.state
        let agentPos = state.searchPoint.Value
        match Map.find agentPos state.dynamicGrid with
        | Agent agent -> (agent, agentPos), List.map (fun n -> n.state.searchPoint.Value) s
        | _ -> failwith "search should always lead to an agent..." 
    | None -> // grid.ToColorRep () |> cprintLines
              failwith "NoBoxToAgentPathFound"

// Its an obstacle if the agent cannot remove it itself
let getObstacleFromPath (agentIdx: AgentIdx) (protectedBox: Box option) (state: Grid) (path: Pos list): Pos option = 
    path 
    |> List.tryFind (fun pos -> 
        match Map.find pos state.dynamicGrid with 
        | Box box -> 
            match protectedBox with
            | Some b' -> box <> b'
            | None -> true
        | Agent a -> 
            agentIdx <> getAgentIdx a
        | _ -> false)

// pos of all DEmpty pos
let freePos (state: Grid): Set<Pos> =
    Map.toSeq state.dynamicGrid
    |> Seq.filter (fun (_, t) -> match t with | DEmpty -> true | _ -> false)
    |> Seq.map fst
    |> Set.ofSeq
 
let getActionsAndResultingState (solution: Node<Grid,Action []> list) = 
    List.rev (List.map (fun n -> n.action) solution), solution.Head.state

let getActionsAndResultingState' (solution: Node<Grid,ActionMeta> list): (ActionMeta) list * Grid = 
    let actions = List.map (fun n -> n.action) solution |> List.rev
    actions, solution.Head.state

let rec clearObstacle prevH agentColorToId (reservedPath: Set<Pos>) (pos: Pos) (state: Grid): ActionList * Grid = 
    let freePositions = 
      reservedPath
      |> Set.difference (freePos state) 

    match Map.find pos state.dynamicGrid with
    | Box box -> 
        let boxPos = pos

        let goalTest agentIdx s = 
            let agentPos = Map.find agentIdx s.agentPos
            let boxPos = Map.find (getId box) s.boxPos
            freePositions.Contains agentPos && freePositions.Contains boxPos

        let box, agent, (grid', actions) = createClearPathFromBox prevH agentColorToId (box, boxPos) [] state
        match new AStarSokobanProblem (getId box, getAgentIdx agent, grid', prevH, goalTest) |> graphSearch with
        | Some solution -> 
            let acts, s = getActionsAndResultingState' solution
            actions @ acts, s
        | None -> failwith "come on"
    | Agent a -> 
        let goalTest s = 
            let agentPos = pos
            let onGoal =
                match Map.find agentPos s.staticGrid with
                | Goal _ -> true
                | _ -> false
            freePositions.Contains agentPos && not onGoal
            
        let grid', actions = createClearPathForAgent prevH agentColorToId a freePositions state

        let acts, grid'' = 
            BFSSokobanProblem (getAgentIdx a, grid', goalTest) 
            |> graphSearch
            |> Option.get
            |> getActionsAndResultingState'
        actions @ acts, grid''

    | _ -> failwith "Pos should contain Box or Agent obstacle"

// boxPos and agentPos might change during the recursion.. which is not intended
and createClearPathFromBox prevH (agentColorToId: Map<Color,Set<AgentIdx>>) (box, boxPos) goalPath grid = 
    let (agent, _), boxPath = pickAgent (boxPos, getBoxColor box) prevH agentColorToId grid
    let solutionPath = boxPath @ goalPath |> List.tail // Drops Agent pos
    box, agent, clearPath prevH agentColorToId agent (Some box) solutionPath grid

and createClearPathForAgent prevH agentColorToId agent freeSpots grid =
    let agentPos = Map.find (getAgentIdx agent) grid.agentPos
    let agentPath = 
        match FreeSpotPointerProblem (grid, agentPos, freeSpots) |> graphSearch with
        | Some s -> List.map (fun n -> n.state.searchPoint.Value) s
        | None -> failwith "could not clear agent"

    clearPath prevH agentColorToId agent None agentPath grid
    
and clearPath (prevH: Map<(Pos * Pos),int>) (agentColorToId: Map<Color,Set<AgentIdx>>) agent (box: Box option) solutionPath grid =
    let solutionSet = Set.ofList solutionPath
    let rec clearPath' gridAcc solutionAcc = 
        match getObstacleFromPath (getAgentIdx agent) box gridAcc solutionPath with
        | Some obstacle -> 
            let obsActionSolution, gridAcc' = clearObstacle prevH agentColorToId (solutionSet) obstacle gridAcc
            clearPath' gridAcc' (solutionAcc @ obsActionSolution)
        | None -> 
            gridAcc, solutionAcc
    let grid', actions = clearPath' grid []
    grid', actions

and createClearPath prevH boxTypeToId agentColorToId (goalPos, goal) grid = 
    let (box, boxPos), goalPath = pickBox (goalPos, goal) prevH boxTypeToId grid
    createClearPathFromBox prevH agentColorToId (box, boxPos) goalPath grid


let solveGoal (goalPos, goal) prevH boxTypeToId agentColorToId grid : ActionList * Grid = 
        let box, agent, (grid', actions) = createClearPath prevH boxTypeToId agentColorToId (goalPos, goal) grid   
        let nextGoalPos =  None

        match new AStarSokobanProblem ((goalPos,nextGoalPos), getId box, getAgentIdx agent, grid', prevH) |> graphSearch with
        | Some [] ->
          actions,grid'
        | Some solution ->
            let actions', grid = getActionsAndResultingState' solution
            (actions @ actions'), grid
        | None -> failwith "come on"

let rec solveGoals prevH boxTypeToId agentColorToId grid (goalOrder: (Pos * Goal) list) =
    let rec solveGoals' actions grid goals goalOrder = 
        if Set.isEmpty goals
        then actions, grid
        else 
            let goal = goalOrder |> List.find (fun g -> Set.contains g goals)
            let actions', grid = solveGoal goal prevH boxTypeToId agentColorToId grid
            let goals = grid.GetUnsolvedGoals ()
            let actions' = actions' |> List.toArray

            solveGoals' (Array.concat [actions; actions']) grid goals goalOrder
    
    let goals: Set<Pos * Goal> = Set.ofList goalOrder
    
    solveGoals' [||] grid goals goalOrder