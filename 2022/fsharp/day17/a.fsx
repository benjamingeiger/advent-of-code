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

let spawn = List.replicate 1000 [spawnDash; spawnPlus; spawnL; spawnI; spawnSquare] |> List.concat

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

let rec simulateStep grid wind piece =
    let gust = List.head wind
    let wind' = List.tail wind

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
        (wind', newGrid)
    else
        simulateStep grid wind' droppedPiece

let simulatePiece (wind, grid) makePiece =
    let piece = makePiece (grid |> Set.maxElement |> fst)

    simulateStep grid wind piece

let simulateGame spawns wind =
    let result =
        spawns
        |> List.truncate 2022
        |> List.fold simulatePiece (wind, Set.singleton (0, -1))
        |> snd
        |> Set.maxElement
        |> fst

    result

let run input =
    let input' = List.replicate 1000 input |> List.concat
    simulateGame spawn input'

doProcess parseInput run rawInput
