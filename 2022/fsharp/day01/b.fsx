// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

let parseInput rawInput =
    rawInput
    |> splitSeq ((=) "")
    |> Seq.map (Seq.map int)

let run input =
    input
    |> Seq.map (Seq.sum)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum

doProcess parseInput run rawInput
