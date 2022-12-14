// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

type State = Rock | Sand

let swap x1 x2 = if x1 < x2 then (x1, x2) else (x2, x1)

let generateCells row =
    row
    |> List.pairwise
    |> List.collect (fun ((r1, c1), (r2, c2)) ->
        if r1 = r2 then
            let (c1', c2') = swap c1 c2
            [c1'..c2'] |> List.map (fun c -> ((r1, c), Rock))
        elif c1 = c2 then
            let (r1', r2') = swap r1 r2
            [r1'..r2'] |> List.map (fun r -> ((r, c1), Rock))
        else
            failwithf "Invalid path from %A to %A" (r1, c1) (r2, c2))

let parseInput rawInput =
    rawInput
    |> List.map (fun (s : string) ->
        s.Split(" -> ")
        |> Seq.map (fun (s' : string) ->
            s'.Split(',')
            |> Seq.map int 
            |> fun xs -> (Seq.item 1 xs, Seq.item 0 xs)) // swap x and y for row and column
        |> List.ofSeq)
    |> List.collect generateCells
    |> Map.ofList

let rec dropSand maxRow grid count (rSand, cSand) =
    let landingRow =
        [rSand..maxRow + 1]
        |> List.tryFind (fun r -> grid |> Map.tryFind (r + 1, cSand) |> Option.isSome)
        |> Option.defaultValue (maxRow + 1)

    if landingRow >= maxRow + 1 then
        dropSand maxRow (grid |> Map.add (landingRow, cSand) Sand) (count + 1) (0, 500)
    elif (grid |> Map.tryFind (landingRow + 1, cSand - 1) |> Option.isNone) then
        dropSand maxRow grid count (landingRow + 1, cSand - 1)
    elif (grid |> Map.tryFind (landingRow + 1, cSand + 1) |> Option.isNone) then
        dropSand maxRow grid count (landingRow + 1, cSand + 1)
    elif landingRow = 0 && cSand = 500 then
        count
    else
        dropSand maxRow (grid |> Map.add (landingRow, cSand) Sand) (count + 1) (0, 500)

let run input =
    let maxRow = input |> Map.keys |> Seq.maxBy fst |> fst

    dropSand maxRow input 1 (0, 500)

doProcess parseInput run rawInput
