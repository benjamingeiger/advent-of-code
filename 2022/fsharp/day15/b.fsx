// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

let parseInput rawInput =
    rawInput
    |> List.choose (function
        | Regex "^Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)$" [sx; sy; bx; by] ->
            Some (int sx, int sy, int bx, int by)
        | s ->
            printfn "Misparsed line: \"%s\"" s
            None)

let manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

let rec findRing input (sx, sy, bx, by) =
    let radius = manhattan (sx, sy) (bx, by) + 1

    [0..radius]
    |> List.collect (fun dx -> [
        (sx + dx, sy + (radius - dx))
        (sx + dx, sy - (radius - dx))
        (sx - dx, sy + (radius - dx))
        (sx - dx, sy - (radius - dx))])
    |> List.filter (fun (x, y) -> x >= 0 && x <= 4000000 && y >= 0 && y <= 4000000)
    |> List.filter (fun (x, y) ->
        input 
        |> List.forall (fun (sx', sy', bx', by') ->
            manhattan (sx', sy') (x, y) > manhattan (sx', sy') (bx', by')))

let run input =
    input
    |> List.collect (findRing input)
    |> List.distinct
    |> List.exactlyOne
    |> fun (x, y) -> (bigint x * 4000000I) + (bigint y)

doProcess parseInput run rawInput
