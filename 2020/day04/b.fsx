// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

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

let findFields (record : string) = record.Split ' ' |> Array.sort

let splitField (s : string) =
    let parts = s.Split ':'

    parts.[0], parts.[1]

let validate r =
    let validate_byr (n, v) =
        match n with
        | "byr" ->
            let value = (int v)
            value >= 1920 && value <= 2002
        | _ -> false

    let validate_iyr (n, v) =
        match n with
        | "iyr" ->
            let value = (int v)
            value >= 2010 && value <= 2020
        | _ -> false

    let validate_eyr (n, v) =
        match n with
        | "eyr" ->
            let value = (int v)
            value >= 2020 && value <= 2030
        | _ -> false

    let validate_hgt (n, v) =
        match n with
        | "hgt" ->
            let m = Regex.Match(v, "(\d+)(in|cm)")
            let mutable num = 0
            if not (Int32.TryParse((m.Groups.[1].Value), &num)) then
                false
            else
                let units = m.Groups.[2].Value
                match units with
                | "cm" -> num >= 150 && num <= 193
                | "in" -> num >= 59 && num <= 76
                | _ -> false
        | _ -> false

    let validate_hcl (n, v) =
        match n with
        | "hcl" ->
            Regex.IsMatch(v, "#[0-9a-f]{6}")
        | _ -> false

    let validate_ecl (n, v) =
        match n with
        | "ecl" ->
            match v with
            | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
            | _ -> false
        | _ -> false

    let validate_pid (n, v) =
        match n with
        | "pid" ->
            Regex.IsMatch(v, "^\d{9}$")
        | _ -> false

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

