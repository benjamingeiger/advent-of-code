// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

let parseInput rawInput =
    rawInput
    |> indexMapCharacters (fun r c x -> ((r, c), x))
    |> Map.ofSeq

let maxScenicScore treeMap =
    let countUnblocked (r, c) x =
        let rec countUnblockedUp r' n =
            match Map.tryFind (r', c) treeMap with
            | Some x' -> if x' >= x then n + 1 else countUnblockedUp (r' - 1) (n + 1)
            | None -> n

        let rec countUnblockedDown r' n =
            match Map.tryFind (r', c) treeMap with
            | Some x' -> if x' >= x then n + 1 else countUnblockedDown (r' + 1) (n + 1)
            | None -> n

        let rec countUnblockedLeft c' n =
            match Map.tryFind (r, c') treeMap with
            | Some x' -> if x' >= x then n + 1 else countUnblockedLeft (c' - 1) (n + 1)
            | None -> n

        let rec countUnblockedRight c' n =
            match Map.tryFind (r, c') treeMap with
            | Some x' -> if x' >= x then n + 1 else countUnblockedRight (c' + 1) (n + 1)
            | None -> n

        let upDist = countUnblockedUp (r - 1) 0
        let downDist = countUnblockedDown (r + 1) 0
        let leftDist = countUnblockedLeft (c - 1) 0
        let rightDist = countUnblockedRight (c + 1) 0

        upDist * downDist * leftDist * rightDist

    treeMap
    |> Map.map countUnblocked
    |> Map.values
    |> Seq.max

let run input =
    maxScenicScore input

doProcess parseInput run rawInput
