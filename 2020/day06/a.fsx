// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let splitInput input =
    let chunk (chunks, curChunk) line =
        match line with
        | "" -> (curChunk :: chunks, [])
        | s -> (chunks, s :: curChunk)

    input
    |> Seq.fold chunk ([], [])
    |> (fun (a, b) -> b :: a)
    |> List.map (List.reduce (+))

input
    |> splitInput
    |> Seq.map (Seq.sort >> Seq.distinct >> Seq.length)
    |> Seq.reduce (+)
    |> printfn "%A"
