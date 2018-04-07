#load "Domain.fs"
#load "Grid.fs"

open System
open System.IO
open Domain
open Grid


let lines = Path.Combine(__SOURCE_DIRECTORY__,"./levels/MAsimple4.lvl") |> readLines
let colors, gridLines = parseColors Map.empty (lines)
let grid = parseMap colors (gridLines |> addIdx)
printfn "%O" grid
