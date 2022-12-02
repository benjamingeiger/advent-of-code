// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

type Throw = Rock | Paper | Scissors

let mapThrow c =
    match c with
    | 'A' | 'X' -> Rock
    | 'B' | 'Y' -> Paper
    | 'C' | 'Z' -> Scissors
    | _ -> failwith "invalid throw type"

let mapScore = function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let mapWin = function
    | (Rock, Rock) -> 3
    | (Paper, Paper) -> 3
    | (Scissors, Scissors) -> 3
    | (Rock, Paper) -> 6
    | (Paper, Scissors) -> 6
    | (Scissors, Rock) -> 6
    | (Rock, Scissors) -> 0
    | (Scissors, Paper) -> 0
    | (Paper, Rock) -> 0

let mapRound (theirs, mine) = mapScore mine + mapWin (theirs, mine)

let parseInput rawInput =
    rawInput
    |> List.map (fun (s : string) -> (mapThrow s.[0], mapThrow s.[2]))

let run input =
    input
    |> List.map mapRound
    |> List.sum

doProcess parseInput run rawInput
