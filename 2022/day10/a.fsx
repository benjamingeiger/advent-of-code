// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

type Instruction =
    | NoOp
    | AddX of int

let rawInput = readLines "input.txt" |> List.ofSeq

let parseInput rawInput =
    rawInput
    |> Seq.choose (function
        | Regex "noop" [] -> Some NoOp
        | Regex "addx (-?\d+)" [value] -> Some (AddX (int value))
        | _ -> None)

let doStep (i, x) = function
    | NoOp -> i + 1, x
    | AddX value -> i + 2, x + value

let lookup x map =
    Map.tryFind x map |> Option.orElse (Map.tryFind (x - 1) map) |> Option.defaultValue 0

let run input =
    let values =
        input
        |> Seq.scan doStep (1, 1)
        |> Map.ofSeq

    let significant = [20; 60; 100; 140; 180; 220]

    significant
    |> Seq.map (fun i -> i * (lookup i values))
    |> Seq.sum

doProcess parseInput run rawInput
