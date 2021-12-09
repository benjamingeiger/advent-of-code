// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input =
    readLines "input.txt"
    |> Seq.map Seq.indexed
    |> Seq.indexed
    |> Seq.collect (fun (r, rs) -> rs |> Seq.map (fun (c, x) -> ((r, c), int x - int '0')))
    |> Map.ofSeq

printfn "%A" input

let neighbors (r, c) =
    [
        (r + 1, c);
        (r - 1, c);
        (r, c + 1);
        (r, c - 1)
    ]

let lowPoints map =
    map
    |> Map.toSeq
    |> Seq.filter (fun ((r, c), d) ->
        neighbors (r, c)
        |> Seq.choose (fun (r', c') -> Map.tryFind (r', c') map)
        |> Seq.map (fun d' -> d' > d)
        |> Seq.fold (&&) true)

let result =
    input
    |> lowPoints
    |> Seq.map snd
    |> Seq.map ((+) 1)
    |> Seq.sum

printfn "%A" result
