// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input =
    readLines "input.txt"
    |> indexCharacters (fun x -> int x - int '0')
    |> Map.ofSeq

let neighbors (r, c) = [ (r + 1, c); (r - 1, c); (r, c + 1); (r, c - 1) ]

let lowPoints map =
    map
    |> Map.toSeq
    |> Seq.filter (fun ((r, c), d) ->
        neighbors (r, c)
        |> Seq.choose (fun (r', c') -> Map.tryFind (r', c') map)
        |> Seq.map (fun d' -> d' > d)
        |> Seq.fold (&&) true)

let flood = bfs { floodFunctions with isValidNeighbor = fun _ value -> value < 9 }

let result =
    input
    |> lowPoints
    |> Seq.map (fun (pos, depth) -> flood input (pos, Some depth) |> Seq.length)
    |> Seq.sortByDescending id
    |> Seq.take 3
    |> Seq.fold ( * ) 1

printfn "%A" result
