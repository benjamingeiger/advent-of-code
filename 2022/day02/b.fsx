// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

type Throw = Rock | Paper | Scissors
type Target = Lose | Draw | Win

let mapThrow = function
    | 'A' -> Rock
    | 'B' -> Paper
    | 'C' -> Scissors
    | _ -> failwith "invalid throw type"

let mapTarget = function
    | 'X' -> Lose
    | 'Y' -> Draw
    | 'Z' -> Win
    | _ -> failwith "invalid target type"

let mapScore = function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let mapPlay = function
    | (Rock, Lose) -> Scissors
    | (Rock, Draw) -> Rock
    | (Rock, Win) -> Paper
    | (Paper, Lose) -> Rock
    | (Paper, Draw) -> Paper
    | (Paper, Win) -> Scissors
    | (Scissors, Lose) -> Paper
    | (Scissors, Draw) -> Scissors
    | (Scissors, Win) -> Rock

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
    |> List.map (fun (s : string) -> (mapThrow s.[0], mapTarget s.[2]))
    |> List.map (fun (theirs, target) -> (theirs, mapPlay (theirs, target)))

let run input =
    input
    |> List.map mapRound
    |> List.sum

doProcess parseInput run rawInput
