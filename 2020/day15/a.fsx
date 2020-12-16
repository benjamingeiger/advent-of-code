// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic 

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.head |> fun s -> s.Split ',' |> Seq.map int |> List.ofSeq |> List.rev

let inputLength = Seq.length input

let age l =
    match List.tryFindIndex ((=) (List.head l)) (List.tail l) with
    | Some n -> n + 1
    | None -> 0

let rec solve hist =
    if (List.length hist) = 2020 then hist
    else solve ((age hist) :: hist)

printfn "%A" (solve input)
