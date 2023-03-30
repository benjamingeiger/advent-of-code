// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

type Direction =
    | Left
    | Right
    | Up
    | Down

type Cell =
    | Wall
    | Empty
    | Blizzard of Direction

type Point = {
    R: int
    C: int
}

let parseInput rawInput =
    let rawCells = 
        rawInput
        |> indexMapCharacters (fun r c x ->
            ({ R = r; C = c },
            match x with
            | '#' -> Wall
            | '<' -> Blizzard Left
            | '>' -> Blizzard Right
            | '^' -> Blizzard Up
            | 'v' -> Blizzard Down
            | _ -> Empty))
        |> Seq.cache

    let blizzards = 
        rawCells
        |> Seq.choose (fun (pos, x) ->
            match x with
            | Blizzard d -> Some (pos, d)
            | _ -> None)
        |> List.ofSeq

    let squares =
        rawCells
        |> Seq.filter (fun (_, cell) -> match cell with | Blizzard _ -> true | Empty -> true | Wall -> false)
        |> Seq.map fst
        |> List.ofSeq

    let grid = (squares, blizzards)

    let startPos =
        rawCells
        |> Seq.filter (fun (_, x) -> x = Empty)
        |> Seq.minBy (fun (pos, _) -> pos.R)
        |> fst

    let endPos =
        rawCells
        |> Seq.filter (fun (_, x) -> x = Empty)
        |> Seq.maxBy (fun (pos, _) -> pos.R)
        |> fst

    grid, startPos, endPos

let updateBlizzards = fun squares -> memoize (fun blizzards ->
    let newBlizzards =
        blizzards
        |> List.map (fun (pos, direction) ->
            match direction with
            | Left ->
                let newPos = { pos with C = pos.C - 1 }
                if List.contains newPos squares then
                    (newPos, Left)
                else
                    let wrapped = squares |> List.filter (fun pos' -> pos'.R = pos.R) |> List.maxBy (fun pos' -> pos'.C)
                    (wrapped, Left)
            | Right ->
                let newPos = { pos with C = pos.C + 1 }
                if List.contains newPos squares then
                    (newPos, Right)
                else
                    let wrapped = squares |> List.filter (fun pos' -> pos'.R = pos.R) |> List.minBy (fun pos' -> pos'.C)
                    (wrapped, Right)
            | Up ->
                let newPos = { pos with R = pos.R - 1 }
                if List.contains newPos squares then
                    (newPos, Up)
                else
                    let wrapped = squares |> List.filter (fun pos' -> pos'.C = pos.C) |> List.maxBy (fun pos' -> pos'.R)
                    (wrapped, Up)
            | Down ->
                let newPos = { pos with R = pos.R + 1 }
                if List.contains newPos squares then
                    (newPos, Down)
                else
                    let wrapped = squares |> List.filter (fun pos' -> pos'.C = pos.C) |> List.minBy (fun pos' -> pos'.R)
                    (wrapped, Down))

    newBlizzards)

let bfs squares startingBlizzards start goal =
    let updateBlizzards' = updateBlizzards squares

    let neighbors pos = [
        { pos with R = pos.R + 1 }
        { pos with C = pos.C + 1 }
        { pos with R = pos.R - 1 }
        { pos with C = pos.C - 1 }
        pos
    ]

    let rec step visited = function
        | [] -> failwith "no route found"
        | (i, map, pos) :: queue ->
            if (Set.contains (i, pos) visited) then step visited queue else

            if pos = goal then i else

            let newMap = updateBlizzards' map

            let neighbors' =
                neighbors pos
                |> List.filter (fun pos' -> squares |> List.contains pos' && newMap |> List.exists (fun (pos'', _) -> pos'' = pos') |> not)
                |> List.map (fun pos' -> i + 1, newMap, pos')

            step (Set.add (i, pos) visited) (queue @ neighbors')

    step Set.empty [(0, startingBlizzards, start)]

let run input =
    let ((squares, blizzards), startPos, endPos) = input

    bfs squares blizzards startPos endPos

doProcess parseInput run rawInput
