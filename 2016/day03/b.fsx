// vim: set et ts=4 sw=4 list :

open System

let parseTriangles lines =
    let sortTriangle (a, b, c) =
        let sides = [| a; b; c |]
        let sortedSides = Array.sort sides
        (sortedSides.[0], sortedSides.[1], sortedSides.[2])

    lines
    |> Seq.chunkBySize 3
    |> Seq.map (String.concat " ")
    |> Seq.map (fun (s : string) -> s.Split(" ", StringSplitOptions.RemoveEmptyEntries))
    |> Seq.map (Array.map int)
    |> Seq.collect (fun a -> [(a.[0], a.[3], a.[6]); (a.[1], a.[4], a.[7]); (a.[2], a.[5], a.[8])])
    |> Seq.map sortTriangle

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> parseTriangles

let isValidTriangle (s1, s2, s3) = s3 < (s1 + s2)

let results = input |> Seq.filter isValidTriangle |> Seq.length

printfn "%A" results
