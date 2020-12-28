// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let lineLength = Seq.length (Seq.head input)

let getColumn n = input |> Seq.map (fun l -> l.[n])

let leastCommonCharacter chars =
    chars
    |> Seq.groupBy id
    |> Seq.sortBy (fun (_, xs) -> Seq.length xs)
    |> Seq.head
    |> fst


let result = 
    [0..(lineLength - 1)]
    |> Seq.map getColumn
    |> Seq.map leastCommonCharacter
    |> Seq.map string
    |> String.concat ""

printfn "%A" result


