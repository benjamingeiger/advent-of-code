// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

let splitSack (s : string) =
    let length = s.Length

    let left = s.[..(length/2 - 1)]
    let right = s.[(length/2)..]

    (left, right)

let findMatches (left, right) =
    let left' = left |> Set.ofSeq
    let right' = right |> Set.ofSeq

    Set.intersect left' right' |> Set.toList |> List.exactlyOne

let priority c =
    if c >= 'A' && c <= 'Z' then (int c) - (int 'A') + 27
    elif c >= 'a' && c <= 'z' then (int c) - (int 'a') + 1
    else 0

let parseInput rawInput =
    rawInput

let run input =
    input
    |> List.map (splitSack >> findMatches >> priority)
    |> List.sum

doProcess parseInput run rawInput
