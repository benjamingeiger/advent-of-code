// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let parseLine (s : string) = s.Split() |> Array.map int |> Array.toList
let parse ss = Seq.map parseLine ss |> Seq.toList

let checksum row =
    List.allPairs row row
    |> List.choose (fun (a, b) -> if a > b && a % b = 0 then Some (a / b) else None)
    |> List.exactlyOne

let result = input |> parse |> List.map checksum |> List.sum

printfn "%A" result
