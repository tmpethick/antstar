// Learn more about F# at http://fsharp.org
open System
open System.IO
open Domain
open Grid

[<EntryPoint>]
let main argv =
  let lines = Path.Combine(__SOURCE_DIRECTORY__,"./levels/MAsimple4.lvl") |> readLines
  let colors, gridLines = parseColors Map.empty (lines)
  let grid = parseMap colors (gridLines |> addIdx)
  printfn "%O" grid
  
  printfn "Press any key to exit"
  Console.ReadKey() |> ignore
  0 // return an integer exit code
