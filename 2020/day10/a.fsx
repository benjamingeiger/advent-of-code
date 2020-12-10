// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.map int

let adapterPairs = input |> Seq.sort |> Seq.windowed 2

let gaps = adapterPairs |> Seq.map (fun xs -> xs.[1] - xs.[0])

let oneGaps = gaps |> Seq.filter ((=) 1) |> Seq.length
let threeGaps = gaps |> Seq.filter ((=) 3) |> Seq.length

printfn "%A" ((oneGaps + 1) * (threeGaps + 1))
