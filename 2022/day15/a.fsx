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

let countExclusions y exclusions (sx, sy, bx, by) =
    let radius = manhattan (sx, sy) (bx, by)
    if y - sy > radius || sy - y > radius then
        exclusions
    else
        let width = radius - abs (y - sy)

        (sx - width, sx + width) :: exclusions

let rec collapse segments (x1, x2) =
    match segments with 
    | [] -> [(x1, x2)]
    | (l, r) :: tail ->
        if x1 < l then
            if x2 < l then
                (x1, x2) :: (l, r) :: tail
            elif x2 <= r then
                (x1, r) :: tail
            else
                collapse tail (x1, x2)
        elif x1 <= r then
            if x2 <= r then
                (l, r) :: tail
            else
                collapse tail (l, x2)
        else
            (l, r) :: collapse tail (x1, x2)

let run input =
    let targetY = 2000000

    let exclusions = 
        input
        |> List.fold (countExclusions targetY) []
        |> List.sort
        |> List.fold collapse []
        |> List.map (fun (x1, x2) -> x2 - x1 + 1)
        |> List.sum

    let beacons =
        input
        |> List.choose (fun (_, _, bx, by) -> if by = targetY then Some(bx) else None)
        |> List.distinct
        |> List.length

    exclusions - beacons

doProcess parseInput run rawInput
