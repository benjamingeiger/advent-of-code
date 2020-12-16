// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic 

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.head |> fun s -> s.Split ',' |> Seq.map int |> List.ofSeq

let age m n t =
    match Map.tryFind n m with
    | Some t' -> t - t'
    | None -> 0

let rec solve N m c t =
    if t = N then c
    else solve N (Map.add c t m) (age m c t) (t + 1)

let processedInput =
    [1 .. (List.length input)]
    |> List.zip input
    |> Map.ofList

printfn "%A" (solve 30000000 processedInput (List.last input) (List.length input))
