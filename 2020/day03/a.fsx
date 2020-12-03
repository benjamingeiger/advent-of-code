// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> List.ofSeq

let getLocation (input : string list) (r : int) =
    let c = r * 3
    let c' = c % input.[r].Length

    input.[r].[c']

let countOfTrees =
    [0 .. ((List.length input) - 1)]
    |> List.map (getLocation input)
    |> List.filter (fun c -> c = '#')
    |> List.length

printfn "%A" countOfTrees
