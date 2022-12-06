// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

let allUnique s = (s |> Set.ofSeq |> Set.count) = (s |> Seq.length)

let parseInput rawInput =
    rawInput
    |> Seq.head
    |> Seq.windowed 14
    |> Seq.indexed
    |> Seq.tryFind (fun (_, x) -> allUnique x)

let run input =
    input
    |> function
        | Some (i, x) -> i + 14
        | None -> -1

doProcess parseInput run rawInput
