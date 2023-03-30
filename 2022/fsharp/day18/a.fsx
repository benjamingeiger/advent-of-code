// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

let parseInput rawInput =
    rawInput
    |> List.choose (function
        | Regex "^(-?\d+),(-?\d+),(-?\d+)$" [x; y; z] ->
            Some (int x, int y, int z)
        | x ->
            eprintfn "misparsed: %A" x
            None)
    |> Set.ofList

let countSides cubes =
    let initialSides = Set.count cubes * 6

    let rightCovered =
        cubes
        |> Set.filter (fun (x, y, z) -> cubes |> Set.contains (x + 1, y, z))
        |> Set.count
    let leftCovered =
        cubes
        |> Set.filter (fun (x, y, z) -> cubes |> Set.contains (x - 1, y, z))
        |> Set.count
    let topCovered =
        cubes
        |> Set.filter (fun (x, y, z) -> cubes |> Set.contains (x, y + 1, z))
        |> Set.count
    let bottomCovered =
        cubes
        |> Set.filter (fun (x, y, z) -> cubes |> Set.contains (x, y - 1, z))
        |> Set.count
    let frontCovered =
        cubes
        |> Set.filter (fun (x, y, z) -> cubes |> Set.contains (x, y, z + 1))
        |> Set.count
    let backCovered =
        cubes
        |> Set.filter (fun (x, y, z) -> cubes |> Set.contains (x, y, z - 1))
        |> Set.count

    initialSides - (rightCovered + leftCovered + topCovered + bottomCovered + frontCovered + backCovered)

let run input =
    input
    |> countSides

doProcess parseInput run rawInput
