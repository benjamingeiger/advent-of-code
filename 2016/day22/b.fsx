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

let nodeGraph =
    nodes
    |> Seq.sort
    |> Seq.map (function (Node ((x, y), s, u, a)) -> if s > 200 then (x, y, '#') else if u = 0 then (x, y, '_') else (x, y, '.'))
    |> Seq.groupBy (fun (_, y, _) -> y)
    |> Seq.map snd
    |> Seq.map (Seq.map (fun (_, _, x) -> x))
    |> Seq.map (String.Concat)
    |> String.concat "\n"

printfn "%A" nodeGraph

// From here we just generate the graph and count the squares manually.
// It's the number of spaces to put the hole to the left of the target,
// plus five times the number of squares the data has to move (minus one),
// plus one for the last move: 63 + 5 * 38 + 1


