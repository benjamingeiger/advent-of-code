// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.toList

let vertices =
    input
    |> List.choose (function
        | Regex "^(\w+) \((\d+)\)" [name; weight] -> Some (name)
        | s ->
            printfn "invalid line [%A]" s
            None)
    |> Set.ofList

let edges =
    input
    |> List.map (function
        | Regex "^(\w+).*-> (.*)$" [name; supported] ->
            (supported : string).Split(", ")
            |> Array.toList
            |> List.map (fun s -> (s, name))
        | _ -> [])
    |> List.concat
    |> Set.ofList

let supported =
    edges
    |> Set.map fst

printfn "%A" (Set.difference vertices supported)
