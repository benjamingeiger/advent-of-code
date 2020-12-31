// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let parseLine s =
    let repeatList n l = [1..n] |> List.collect (fun _ -> l)

    let rec go l =
        match l with
        | [] -> []
        | h :: t when h = '(' ->
            let (marker, rest) = List.splitAt (List.findIndex ((=) ')') t) t
            let (lengthText, repsText) = List.splitAt (List.findIndex ((=) 'x') marker) marker
            let length = int (String.concat "" (Seq.map string lengthText))
            let reps = int (String.concat "" (Seq.map string (List.tail repsText)))
            let (repeated, rest') = List.splitAt length (List.tail rest)
            printfn "%A %A %A" repeated reps rest'
            (repeatList reps repeated) @ (go rest')
        | h :: t when h = ')' ->
            go t
        | h :: t ->
            h :: (go t)

    go (Seq.toList s)
    |> List.map string
    |> String.concat ""

input
|> Seq.map parseLine
|> Seq.map Seq.length
|> Seq.sum
|> printfn "%A"
