// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let parseLine s =
    let rec go count l =
        match l with
        | [] -> count
        | h :: t when h = '(' ->
            let (marker, rest) = List.splitAt (List.findIndex ((=) ')') t) t
            let (lengthText, repsText) = List.splitAt (List.findIndex ((=) 'x') marker) marker
            let length = int (String.concat "" (Seq.map string lengthText))
            let reps = bigint.Parse (String.concat "" (Seq.map string (List.tail repsText)))
            let (repeated, rest') = List.splitAt length (List.tail rest)
            printfn "%A %A %A" repeated reps rest'
            (go (count + (reps * (go 0I repeated))) rest')
        | h :: t when h = ')' ->
            go count t
        | h :: t ->
            go (count + 1I) t

    go 0I (Seq.toList s)

input
|> Seq.map parseLine
|> Seq.sum
|> printfn "%A"
