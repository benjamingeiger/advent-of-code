// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.head

let sequenceLength s =
    let step remaining current =
        let doesMatch c1 c2 =
            Char.ToUpper(c1) = Char.ToUpper(c2) && (Char.IsUpper(c1) && Char.IsLower(c2) || Char.IsLower(c1) && Char.IsUpper(c2))

        match remaining with
        | [] ->
            [current]
        | c :: rest ->
            if doesMatch c current
            then
                rest
            else
                current :: remaining

    Seq.fold step [] s |> Seq.length

let removeChar c s =
    s 
    |> Seq.filter (fun c' -> Char.ToUpper(c') <> Char.ToUpper(c))
    |> Seq.toArray
    |> System.String

let allPolymers s =
    s
    |> Seq.map (fun c -> Char.ToUpper(c))
    |> Seq.distinct
    |> Seq.cache

input
|> allPolymers
|> Seq.map (fun c -> removeChar c input)
|> Seq.map (fun c -> (c, sequenceLength c))
|> Seq.minBy snd
|> snd
|> printfn "%A"
