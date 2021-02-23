// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readLines filePath = System.IO.File.ReadLines filePath
let input =
    readLines "input.txt"
    |> Seq.choose (function
        | Regex "^(\d+): (\d+)$" [layer; range] -> Some (int layer, int range)
        | s ->
            printfn "Invalid rule [%A]" s
            None)

let isCaughtAt (layer, range) =
    let pos = layer % (2 * (range - 1))
    pos = 0

let result =
    input
    |> Seq.filter isCaughtAt
    |> Seq.map (fun (layer, range) -> layer * range)
    |> Seq.sum

printfn "%A" result


