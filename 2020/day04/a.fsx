// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let splitInput input =
    let chunk (chunks, curChunk) line =
        match line with
        | "" -> (curChunk :: chunks, [])
        | s -> (chunks, s :: curChunk)

    input
    |> Seq.fold chunk ([], [])
    |> (fun (a, b) -> b :: a)
    |> List.map (List.reduce (fun x y -> x + " " + y))

let findFields (record : string) =
    let segments = record.Split ' ' |> Array.sort

    segments

let splitField (s : string) =
    let parts = s.Split ':'

    parts.[0], parts.[1]

let validate r =
    let validate_byr (n, v) = n = "byr"
    let validate_iyr (n, v) = n = "iyr"
    let validate_eyr (n, v) = n = "eyr"
    let validate_hgt (n, v) = n = "hgt"
    let validate_hcl (n, v) = n = "hcl"
    let validate_ecl (n, v) = n = "ecl"
    let validate_pid (n, v) = n = "pid"

    (Seq.exists validate_byr r
    && Seq.exists validate_iyr r
    && Seq.exists validate_eyr r
    && Seq.exists validate_hgt r
    && Seq.exists validate_hcl r
    && Seq.exists validate_ecl r
    && Seq.exists validate_pid r)


let chunks = splitInput input

let fieldGroups = 
    chunks
    |> Seq.map findFields
    |> Seq.map Array.sort
    |> Seq.map (Seq.map splitField)
    |> Seq.filter validate
    |> Seq.length

printfn "%A" fieldGroups

