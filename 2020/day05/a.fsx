// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let parseSeat (row, col) c =
    match c with
    | 'B' -> (row * 2 + 1, col)
    | 'F' -> (row * 2, col)
    | 'L' -> (row, col * 2)
    | 'R' -> (row, col * 2 + 1)
    | _ -> (row, col)

let passes =
    input
    |> Seq.map (Seq.fold parseSeat (0, 0))
    |> Seq.map (fun (r, c) -> r * 8 + c)

printfn "%A" passes
printfn "%A" (Seq.max passes)
