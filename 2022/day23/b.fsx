// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

let parseInput rawInput =
    rawInput
    |> indexMapCharacters (fun r c x -> if x = '#' then Some (r, c) else None)
    |> Seq.choose id
    |> Set.ofSeq

let northNeighbors (r, c) =
    [
        (r - 1, c)
        (r - 1, c + 1)
        (r - 1, c - 1)
    ] |> Set.ofList

let southNeighbors (r, c) =
    [
        (r + 1, c)
        (r + 1, c + 1)
        (r + 1, c - 1)
    ] |> Set.ofList

let westNeighbors (r, c) =
    [
        (r, c - 1)
        (r + 1, c - 1)
        (r - 1, c - 1)
    ] |> Set.ofList

let eastNeighbors (r, c) =
    [
        (r, c + 1)
        (r + 1, c + 1)
        (r - 1, c + 1)
    ] |> Set.ofList

let allNeighbors (r, c) = Set.unionMany [northNeighbors (r, c); southNeighbors (r, c); eastNeighbors (r, c); westNeighbors (r, c)]

let considerationOrder =
    [
        (northNeighbors, (fun (r, c) -> ((r, c), (r - 1, c))))
        (southNeighbors, (fun (r, c) -> ((r, c), (r + 1, c))))
        (westNeighbors, (fun (r, c) -> ((r, c), (r, c - 1))))
        (eastNeighbors, (fun (r, c) -> ((r, c), (r, c + 1))))
    ]

let rec doStep count order state = 
    let consider pos =
        if Set.intersect state (allNeighbors pos) = Set.empty then (pos, pos)
        else
            order
            |> List.tryPick (fun (neighbors, pick) ->
                if Set.intersect (neighbors pos) state = Set.empty then
                    Some (pick pos) else None)
            |> Option.defaultValue (pos, pos)

    let proposals = state |> Set.map consider

    let proposedMotions =
        proposals 
        |> Set.filter (fun (cur, next) -> cur <> next)
        |> Set.toList
        |> List.groupBy snd
        |> List.filter (fun (_, motions) -> List.length motions = 1)
        |> List.map (fun (_, motions) -> (motions |> List.exactlyOne))

    if proposedMotions = [] then count else

    let (removals, additions) = proposedMotions |> List.unzip

    let newState = Set.union (Set.difference state (Set.ofList removals)) (Set.ofList additions)

    doStep (count + 1) (List.tail order @ [List.head order]) newState

let run input =
    input
    |> doStep 1 considerationOrder

doProcess parseInput run rawInput
