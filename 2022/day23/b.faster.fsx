// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

type Point = { R: int; C: int }

let parseInput rawInput =
    rawInput
    |> indexMapCharacters (fun r c x -> if x = '#' then Some { R = r; C = c } else None)
    |> Seq.choose id
    |> Set.ofSeq

let northNeighbors pt =
    [
        { R = pt.R - 1; C = pt.C }
        { R = pt.R - 1; C = pt.C + 1 }
        { R = pt.R - 1; C = pt.C - 1 }
    ] |> Set.ofList

let southNeighbors pt =
    [
        { R = pt.R + 1; C = pt.C }
        { R = pt.R + 1; C = pt.C + 1 }
        { R = pt.R + 1; C = pt.C - 1 }
    ] |> Set.ofList

let westNeighbors pt =
    [
        { R = pt.R; C = pt.C - 1 }
        { R = pt.R + 1; C = pt.C - 1 }
        { R = pt.R - 1; C = pt.C - 1 }
    ] |> Set.ofList

let eastNeighbors pt =
    [
        { R = pt.R; C = pt.C + 1 }
        { R = pt.R + 1; C = pt.C + 1 }
        { R = pt.R - 1; C = pt.C + 1 }
    ] |> Set.ofList

let allNeighbors pt = Set.unionMany [northNeighbors pt; southNeighbors pt; eastNeighbors pt; westNeighbors pt]

let considerationOrder =
    [
        (northNeighbors, (fun pt -> (pt, { pt with  R = pt.R - 1 })))
        (southNeighbors, (fun pt -> (pt, { pt with R = pt.R + 1 })))
        (westNeighbors, (fun pt -> (pt, { pt with C = pt.C - 1 })))
        (eastNeighbors, (fun pt -> (pt, { pt with C = pt.C + 1 })))
    ]

let rec doStep count order state = 
    printfn "%A" count
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
