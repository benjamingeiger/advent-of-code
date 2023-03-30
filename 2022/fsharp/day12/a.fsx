// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

let parseInput rawInput =
    rawInput
    |> indexMapCharacters (fun r c x -> ((r, c), x))
    |> Map.ofSeq

let bfs map start goal =

    let getSquare (r, c) =
        let cur' = map |> Map.tryFind (r, c) |> Option.defaultValue (char ((int 'z') + 2))
        if cur' = 'S' then 'a' elif cur' = 'E' then 'z' else cur'

    let neighbors ((r, c), n) =
        let cur = getSquare (r, c)

        [(r + 1, c); (r - 1, c); (r, c + 1); (r, c - 1)]
        |> List.choose (fun (r', c') -> if (int (getSquare (r', c'))) <= (int cur) + 1 then Some ((r', c'), n + 1, (r, c)) else None)

    let rec step searched = function
        | [] -> failwith "couldn't find a route"
        | ((r, c), n, prev) :: queue ->
            if (r, c) = goal then (n, Map.add (r, c) prev searched)
            elif Map.containsKey (r, c) searched then
                step searched queue
            else
                step (Map.add (r, c) prev searched) (queue @ neighbors ((r, c), n))

    step Map.empty [(start, 0, start)]

let run input =
    let start = input |> Map.findKey (fun _ v -> v = 'S')
    let goal = input |> Map.findKey (fun _ v -> v = 'E')

    bfs input start goal |> fst


doProcess parseInput run rawInput
