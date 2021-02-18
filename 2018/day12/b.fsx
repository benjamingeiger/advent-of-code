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
        |> Seq.mapi (fun i c -> (bigint i, if c = '#' then true else false))
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

let step rules (generation, state) =
    if generation % 1000000I = 0I then printfn "%A" generation

    let lowerBound = (Set.minElement state) - 2I
    let upperBound = (Set.maxElement state) + 2I

    [lowerBound..upperBound]
    |> Seq.map (fun i -> (i, ((Set.contains (i - 2I) state),
                              (Set.contains (i - 1I) state),
                              (Set.contains (i + 0I) state),
                              (Set.contains (i + 1I) state),
                              (Set.contains (i + 2I) state))))
    |> Seq.filter (fun (i, w) -> Map.find w rules)
    |> Seq.map fst
    |> Set.ofSeq
    |> fun s -> Some ((generation + 1I, s), (generation + 1I, s))

let dump state =
    let lowerBound = (Set.minElement state) - 2I
    let upperBound = (Set.maxElement state) + 2I

    [lowerBound..upperBound]
    |> Seq.map (fun i -> if Set.contains i state then "#" else ".")
    |> String.concat ""

let result = 
    Seq.unfold (step rules) (0I, initialState)
    |> Seq.take 1000
    |> Seq.map (fun (gen, state) -> (state |> Set.toList |> List.sum, dump state))

Seq.iter (fun s -> printfn "%A" s) result

// Solved by hand:
    // Generation 1000 = 63905
    // Each generation adds 63 to the sum
    //  (63 live plants, each shifts one place to the right)
    //  So, (50000000000 - 1000) * 63 + 63905 = 3150000000905

let numGenerations = 50000000000I - 1000I
let differencePerGeneration = 63I
let totalAt1000 = 63905I

printfn "%A" (numGenerations * differencePerGeneration + totalAt1000)
