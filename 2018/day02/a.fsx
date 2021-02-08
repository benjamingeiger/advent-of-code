// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let frequencies = Seq.countBy id

let candidates =
    input
    |> Seq.map frequencies
    |> Seq.filter (Seq.exists (fun (c, n) -> n = 2 || n = 3))
    |> Seq.cache

let candidates2 = candidates |> Seq.filter (Seq.exists (fun (c, n) -> n = 2)) |> Seq.length
let candidates3 = candidates |> Seq.filter (Seq.exists (fun (c, n) -> n = 3)) |> Seq.length

printfn "%A" (candidates2 * candidates3)
