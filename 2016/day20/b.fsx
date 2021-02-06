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

printfn "%A" pairs

let merge existing (l, h) =
    match existing with
    | [] -> [(l, h)]
    | (l', h') :: xs ->
        if l > 0u && (l - 1u) > h' then
            (l, h) :: (l', h') :: xs
        else
            ((min l l'), (max h h')) :: xs

let ranges = Seq.fold merge [] pairs |> List.rev

let measureGap rangepair =
    match rangepair with
    | [(l1, h1); (l2, h2)] ->
        if h1 > l2 then failwith (sprintf "bad pair %A" rangepair)
        l2 - h1 - 1u
    | _ -> failwith "whoops"

let inverted =
    ranges
    |> List.windowed 2
    |> List.map measureGap
    |> List.sum

printfn "%A" inverted
