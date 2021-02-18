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

let parseInput input =
    let initialState =
        input
        |> Seq.head
        |> Seq.skip (String.length "initial state: ")
        |> Seq.mapi (fun i c -> (i, if c = '#' then true else false))
        |> Seq.choose (fun (i, c) -> if c then Some i else None)
        |> Set.ofSeq

    let rules =
        input
        |> Seq.skip 2
        |> Seq.choose (function
            | Regex "(.)(.)(.)(.)(.) => (.)" [p2; p1; c; n1; n2; r] ->
                Some (((if p2 = "#" then true else false),
                       (if p1 = "#" then true else false),
                       (if c  = "#" then true else false),
                       (if n1 = "#" then true else false),
                       (if n2 = "#" then true else false)),
                      (if r  = "#" then true else false))
            | s ->
                printfn "invalid rule [%A]" s
                None)
        |> Map.ofSeq

    initialState, rules

let (initialState, rules) = parseInput input

let step rules state =
    let lowerBound = (Set.minElement state) - 2
    let upperBound = (Set.maxElement state) + 2

    [lowerBound..upperBound]
    |> Seq.map (fun i -> (i, ((Set.contains (i - 2) state),
                              (Set.contains (i - 1) state),
                              (Set.contains (i + 0) state),
                              (Set.contains (i + 1) state),
                              (Set.contains (i + 2) state))))
    |> Seq.filter (fun (i, w) -> Map.find w rules)
    |> Seq.map fst
    |> Set.ofSeq
    |> fun s -> Some (s, s)

let result = Seq.unfold (step rules) initialState |> Seq.take 20 |> Seq.last |> Set.toList |> List.sum
printfn "%A" result


