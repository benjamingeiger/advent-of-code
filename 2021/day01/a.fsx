// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.map Int32.Parse |> Seq.cache

let result =
    input
    |> Seq.windowed 2
    |> Seq.filter (fun p -> p.[1] > p.[0])
    |> Seq.length

printfn "%A" result


