// Learn more about F# at http://fsharp.org
module Program
open System
open System.IO
open Domain
open Grid
open Search

let rec testActions state = function
    | action :: actions ->
        match apply action state with
        | Error err      -> printfn "%O" err
        | Success state' -> printfn "%O" state'
                            testActions state' actions
    | [] -> printfn "Is Goal state: %O" (allGoalsMet state)

[<EntryPoint>]
let main argv =
  let problem = SokobanProblem ("./levels/SAminimal.lvl", Search.allGoalsMet)

  printfn "%O" problem.Initial
  Grid.allValidActions '0' problem.Initial |> List.map (fun (_, s') -> printfn "%O" s') |> ignore

  let actions = [Move ('0', E); Move ('0', E); Push ('0', E, E)]
  testActions problem.Initial actions

  match graphSearch problem with
  | Some solution -> solution 
                     |> Seq.map (fun n -> n.state) 
                     |> Seq.iter (printf "%O")
  | None -> printf "No solution found"

  printfn "Press any key to exit"
  Console.ReadKey() |> ignore
  0 // return an integer exit code
