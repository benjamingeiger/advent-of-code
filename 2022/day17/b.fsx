// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

type Direction = Left | Right

// borrowed from https://www.fssnip.net/a5/title/Generate-a-repeating-infinite-sequence-
let rec cycle xs = seq { yield! xs; yield! cycle xs }

let spawnDash r = [(r + 4, 2); (r + 4, 3); (r + 4, 4); (r + 4, 5)]
let spawnPlus r = [(r + 4, 3); (r + 5, 2); (r + 5, 3); (r + 5, 4); (r + 6, 3)]
let spawnL r = [(r + 4, 2); (r + 4, 3); (r + 4, 4); (r + 5, 4); (r + 6, 4)]
let spawnI r = [(r + 4, 2); (r + 5, 2); (r + 6, 2); (r + 7, 2)]
let spawnSquare r = [(r + 4, 2); (r + 5, 2); (r + 4, 3); (r + 5, 3)]

let spawn = List.replicate 2000 [spawnDash; spawnPlus; spawnL; spawnI; spawnSquare] |> List.concat

let parseInput rawInput =
    rawInput
    |> List.head
    |> Seq.choose (function
        | '<' -> Some Left
        | '>' -> Some Right
        | x ->
            eprintfn "invalid character %c" x
            None)
    |> List.ofSeq

let collides grid piece =
    piece
    |> List.exists (fun (r, c) -> (c < 0 || c > 6 || Set.contains (r, c) grid || r < 1))

let rec simulateStep grid windCount wind piece =
    let gust = List.head wind
    let wind' = List.tail wind
    let windCount' = windCount + 1

    let shiftedPiece =
        match gust with
        | Left ->
            let piece' = piece |> List.map (fun (r, c) -> (r, c - 1))
            if collides grid piece' then piece else piece'
        | Right ->
            let piece' = piece |> List.map (fun (r, c) -> (r, c + 1))
            if collides grid piece' then piece else piece'

    let droppedPiece = shiftedPiece |> List.map (fun (r, c) -> (r - 1, c))

    if collides grid droppedPiece then
        let newGrid = Set.union (Set.ofList shiftedPiece) grid
        ((windCount', wind'), newGrid)
    else
        simulateStep grid windCount' wind' droppedPiece

let simulatePiece ((windCount, wind), grid) makePiece =
    let piece = makePiece (grid |> Set.maxElement |> fst)

    simulateStep grid windCount wind piece

let simulateGame spawns wind =
    let results =
        spawns
        |> List.scan simulatePiece ((0, wind), Set.singleton (0, -1))
        |> List.map (fun ((windCount, _), grid) -> (windCount, grid))
        |> List.indexed

    // let maxRow = result |> Set.maxElement |> fst

    // [1..maxRow]
    // |> List.filter (fun r -> Set.isProperSubset ([(r, 0); (r, 1); (r, 2); (r, 3); (r, 4); (r, 5); (r, 6)] |> Set.ofList) result)
    // // |> List.iter (printfn "solid row at row %A")
    // |> List.pairwise
    // |> List.iter (fun (r1, r2) -> printfn "solid rows at %d and %d (diff %d)" r1 r2 (r2 - r1))

    results

let run input =
    let input' = List.replicate 1000 input |> List.concat
    let results = simulateGame spawn input'

    // sanity check using part 1
    results
        |> List.find (fun (i, _) -> i = 2022)
        |> snd |> snd
        |> Set.maxElement
        |> fst
        |> fun x -> if x <> 3100 then failwithf "wrong result for part 1: %d" x else ()

    printfn "Results generated"

    let standardizeGrid grid =
        let maxRow = grid |> Set.maxElement |> fst

        grid 
        |> Set.filter (fun (r, _) -> r + 100 >= maxRow)
        |> Set.map (fun (r, c) -> (r - maxRow, c))

    // find the cycle lengths and starting points.
    // note that once the cycle starts, _every_ piece will look like the start
    // of the cycle, so the first potential cycle length that starts repeating is
    // the real one.

    let cycleResults =
        results
        |> List.groupBy (fun (index, (windCount, _)) -> (index % 5, windCount % (List.length input)))
        |> List.filter (fun (_, gs) -> List.length gs > 3)
        |> List.map (fun ((indexModulo, windCountModulo), result) -> ((indexModulo, windCountModulo, (result |> List.item 1 |> fst)), result))
        |> List.map (fun (key, result) -> 
            (key, 
            result
            |> List.tail
            |> List.map (fun (index, (_, g)) -> (index, g |> standardizeGrid))
            |> List.pairwise
            |> List.choose (fun ((i1, g1), (i2, g2)) -> if g1 = g2 then Some (i2 - i1, g1) else None)
            |> List.distinct))
        |> List.truncate 10

    // printfn "%A" cycleResults
    // result: cycle length 1735, starting at piece 1872.

    // note: since any piece after 1872 could be the start of the cycle, I'm going to pretend that
    // the cycle actually starts an even number of cycles before 1e12 so I don't have to worry about
    // adding more pieces at the end.

    let numberOfPieces = 1000000000000I
    let fakeCycleStart = (numberOfPieces - 1872I) % 1735I + 1872I

    let startOfCycleHeight =
        results
        |> List.item (int fakeCycleStart)
        |> snd |> snd
        |> Set.maxElement |> fst
        |> bigint

    let endOfCycleHeight =
        results
        |> List.item (int fakeCycleStart + 1735)
        |> snd |> snd
        |> Set.maxElement |> fst
        |> bigint

    let heightIncreasePerCycle = (endOfCycleHeight - startOfCycleHeight)

    let numberOfCycles = (numberOfPieces - fakeCycleStart) / 1735I

    startOfCycleHeight + numberOfCycles * heightIncreasePerCycle

doProcess parseInput run rawInput
