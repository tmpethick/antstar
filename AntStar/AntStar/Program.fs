// Learn more about F# at http://fsharp.org
module Program
open System
open System.IO
open Domain
open Grid
open Search

[<EntryPoint>]
let main argv =
  let lines = Path.Combine(__SOURCE_DIRECTORY__,"./levels/MAsimple4.lvl") |> readLines
  let colors, gridLines = parseColors Map.empty (lines)
  let grid = parseMap colors (gridLines |> addIdx)
  printfn "%O" grid
  let problem = SokobanProblem ("./levels/SAsimple1.lvl", Search.allGoalsMet)
  let solution = graphSearch problem
  
  printfn "Press any key to exit"
  printfn "%A" solution
  Console.ReadKey() |> ignore
  0 // return an integer exit code
