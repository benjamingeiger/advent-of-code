// vim: set et ts=4 sw=4 list :

// open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

let parseInput rawInput =
    rawInput
    |> List.choose (function
        | Regex "^(\d+)-(\d+),(\d+)-(\d+)$" [a1; b1; a2; b2] ->
            Some ((int a1, int b1), (int a2, int b2))
        | _ -> None)

let run input =
    input
    |> List.filter (fun ((a1, b1), (a2, b2)) ->
        not ((a1 < a2 && b1 < a2) || (a2 < a1 && b2 < a1)))
    |> List.length

doProcess parseInput run rawInput
