// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.cache

let equal (x, y) = x = y
let differ (x, y) = x <> y

let differingLetterCount s t = Seq.zip s t |> Seq.filter differ |> Seq.length
let matchingLetters ((s : seq<char>), t) = Seq.zip s t |> Seq.filter equal |> Seq.map fst |> Seq.toArray |> System.String

let nearMatches =
    Seq.allPairs input input
    |> Seq.filter (fun (s, t) -> (differingLetterCount s t) = 1)
    |> Seq.map matchingLetters

printfn "%A" nearMatches
