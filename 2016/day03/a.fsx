// vim: set et ts=4 sw=4 list :

open System

let parseLine (s : string) =
    let parts = s.Trim().Split(" ", StringSplitOptions.RemoveEmptyEntries)
    let intParts = parts |> Array.map int |> Array.sort
    (int intParts.[0]), (int intParts.[1]), (int intParts.[2])

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.map parseLine

let isValidTriangle (s1, s2, s3) = s3 < (s1 + s2)

let results = input |> Seq.filter isValidTriangle |> Seq.length

printfn "%A" results
