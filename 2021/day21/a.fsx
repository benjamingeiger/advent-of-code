// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> List.ofSeq

let parseInput input =
    input
    |> Seq.choose (function
        | Regex "^Player . starting position: (\d+)$" [pos] -> Some (int pos)
        | _ -> None)
    |> (fun xs -> (Seq.item 0 xs, Seq.item 1 xs))

module Seq = let rec cycle xs = seq { yield! xs; yield! cycle xs }

let doTurnSequence ((num1, pos1, score1), (num2, pos2, score2)) roll =
    let moveToken pos roll =
        let newPos = (pos + roll) % 10
        if newPos = 0 then 10 else newPos

    let pUpdated =
        roll
        |> moveToken pos1
        |> (fun newPos1 -> (num1, newPos1, newPos1 + score1))

    ((num2, pos2, score2), pUpdated)

let doGame p1 p2 die =
    die
    |> Seq.chunkBySize 3
    |> Seq.map Array.sum
    |> Seq.scan doTurnSequence ((1, p1, 0), (2, p2, 0))

let result =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let (start1, start2) = parseInput input

    let die = [1..100] |> Seq.cycle

    let gameSequence = doGame start1 start2 die |> Seq.takeWhile (fun ((_, _, _), (_, _, x)) -> x < 1000)

    Seq.iter (printfn "%A") gameSequence

    let totalRolls = Seq.length gameSequence * 3
    let loserScore = gameSequence |> Seq.last |> fun ((_, _, _), (_, _, x)) -> x

    let returnValue =
        (totalRolls, loserScore, totalRolls * loserScore, gameSequence)

    stopwatch.Stop()
    printfn "Elapsed milliseconds: %f" stopwatch.Elapsed.TotalMilliseconds

    returnValue

printfn "%A" result
