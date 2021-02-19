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

let isValid (pp : string) =
    let parts = pp.Split()

    parts
    |> Array.toList
    |> List.map (Seq.toList >> List.sort)
    |> List.groupBy id
    |> List.exists (fun (k, v) -> List.length v > 1)
    |> not

input
|> Seq.filter isValid
|> Seq.length
|> printfn "%A" 

