// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic 

let enumerateSeq s = Seq.zip (Seq.initInfinite id) s

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

type Direction = East | SouthEast | SouthWest | West | NorthWest | NorthEast

let parseDirectionLine s =
    let rec go dirs = function
        | [] -> dirs
        | 'e' :: t -> go (East :: dirs) t
        | 'n' :: 'e' :: t -> go (NorthEast :: dirs) t
        | 'n' :: 'w' :: t -> go (NorthWest :: dirs) t
        | 'w' :: t -> go (West :: dirs) t
        | 's' :: 'w' :: t -> go (SouthWest :: dirs) t
        | 's' :: 'e' :: t -> go (SouthEast :: dirs) t
        | _ -> failwith "invalid input line"

    s
        |> List.ofSeq
        |> go []
        |> List.rev

let steps = Seq.map parseDirectionLine input |> Seq.toList

let findTile ss =
    let oneStep = function
        | East -> (+0, +1)
        | SouthEast -> (-1, +1)
        | SouthWest -> (-1, +0)
        | West -> (+0, -1)
        | NorthWest -> (+1, -1)
        | NorthEast -> (+1, +0)

    let add (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

    ss
        |> List.map oneStep
        |> List.reduce add

let flips =
    steps
        |> List.map findTile
        |> List.sort
        |> List.countBy id
        |> List.filter (fun (_, c) -> c % 2 <> 0)

printfn "%A" flips
printfn "%A" (List.length flips)
