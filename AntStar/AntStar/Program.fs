// Learn more about F# at http://fsharp.org
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

    printfn "Press any key to exit"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
