// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.skip 2

type Node = Node of (int * int) * int * int * int

let parseLine = function
    | Regex "x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T" [x; y; size; used; avail] -> 
        Some (Node ((int x, int y), int size, int used, int avail))
    | x ->
        printfn "Invalid line [%s]" x
        None

let nodes = input |> Seq.map parseLine |> Seq.choose id |> Seq.cache

let result =
    Seq.allPairs nodes nodes
    |> Seq.filter (fun (Node ((x1, y1), s1, u1, a1), Node ((x2, y2), s2, u2, a2)) ->
        (x1, y1) <> (x2, y2) && u1 > 0 && a2 > u1)
    |> Seq.length

printfn "%A" result



