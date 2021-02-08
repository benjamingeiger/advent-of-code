// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

// borrowed from http://www.fssnip.net/a5/title/Generate-a-repeating-infinite-sequence-
module Seq = let rec cycle xs = seq { yield! xs; yield! cycle xs }

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.map int

let adjust (cur, seen) freq = (cur + freq, Set.add cur seen)
let frequencies =
    input
    |> Seq.cache
    |> Seq.cycle
    |> Seq.scan adjust (0, Set.empty)
    |> Seq.pick (fun (cur, seen) -> if Set.contains cur seen then Some (cur) else None)

printfn "%A" frequencies

