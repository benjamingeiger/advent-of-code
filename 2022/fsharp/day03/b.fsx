// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

let findMatches sacks =
    sacks
    |> List.map Set.ofSeq
    |> Set.intersectMany
    |> Set.toList
    |> List.exactlyOne

let priority c =
    if c >= 'A' && c <= 'Z' then (int c) - (int 'A') + 27
    elif c >= 'a' && c <= 'z' then (int c) - (int 'a') + 1
    else 0

let parseInput rawInput =
    rawInput

let run input =
    input
    |> List.chunkBySize 3
    |> List.map (findMatches >> priority)
    |> List.sum

doProcess parseInput run rawInput
