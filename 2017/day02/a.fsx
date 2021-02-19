// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let parseLine (s : string) = s.Split() |> Array.map int |> Array.toList
let parse ss = Seq.map parseLine ss |> Seq.toList

let checksum row = (List.max row) - (List.min row)

let result = input |> parse |> List.map checksum |> List.sum

printfn "%A" result
