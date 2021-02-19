// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.head |> Seq.map (fun c -> int c - int '0') |> List.ofSeq

let rotate n l =
    List.append (List.skip n l) (List.truncate n l)

let pairs = List.zip input (rotate ((List.length input) / 2) input)

let result = pairs |> List.choose (fun (a, b) -> if a = b then Some a else None) |> List.sum

printfn "%A" result
