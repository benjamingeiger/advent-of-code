// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let parseLine (s : string) =
    s.Split("-")
    |> fun a -> (uint a.[0], uint a.[1])

let pairs = 
    input
    |> Seq.map parseLine
    |> Seq.sort

(*printfn "%A" pairs*)

let merge existing (l, h) =
    match existing with
    | [] -> [(l, h)]
    | (l', h') :: xs ->
        if l > (h' + 1u) then (l, h) :: (l', h') :: xs
        else (l', (max h h')) :: xs

let ranges = Seq.fold merge [] pairs

printfn "%A" (ranges |> List.rev |> List.head |> snd |> (+) 1u)
