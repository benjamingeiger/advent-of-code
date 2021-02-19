// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.head |> Seq.map (fun c -> int c - int '0') |> List.ofSeq

input
|> fun l -> (List.last l) :: l
|> List.windowed 2
|> List.choose (function | [a; b] -> (if a = b then Some a else None) | _ -> None)
|> List.sum
|> printfn "%A"
