// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

let parseInput rawInput =
    rawInput
    |> indexMapCharacters (fun r c x -> ((r, c), x))
    |> Map.ofSeq

let countVisible treeMap =
    let isVisibleFromTop r c x =
        treeMap
        |> Map.filter (fun (r', c') x' -> c' = c && r' < r)
        |> Map.exists (fun _ x' -> x' >= x)
        |> not

    let isVisibleFromBottom r c x =
        treeMap
        |> Map.filter (fun (r', c') x' -> c' = c && r' > r)
        |> Map.exists (fun _ x' -> x' >= x)
        |> not

    let isVisibleFromLeft r c x =
        treeMap
        |> Map.filter (fun (r', c') x' -> r' = r && c' < c)
        |> Map.exists (fun _ x' -> x' >= x)
        |> not

    let isVisibleFromRight r c x =
        treeMap
        |> Map.filter (fun (r', c') x' -> r' = r && c' > c)
        |> Map.exists (fun _ x' -> x' >= x)
        |> not

    let isVisible (r, c) x = isVisibleFromTop r c x || isVisibleFromBottom r c x || isVisibleFromLeft r c x || isVisibleFromRight r c x

    treeMap
    |> Map.filter isVisible
    |> Map.count

let run input =
    countVisible input

doProcess parseInput run rawInput
