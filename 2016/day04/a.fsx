// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let roomPattern = @"([A-Za-z-]*)(\d+)\[([A-Za-z]+)\]"
let roomRegex = new Regex(roomPattern)

let parseRoom s =
    let m = roomRegex.Match s

    let name = m.Groups.[1].Value |> Seq.filter ((<>) '-') |> String.Concat
    let number = m.Groups.[2].Value |> int
    let checksum = m.Groups.[3].Value

    (name, number, checksum)

let validateRoom (name, _, checksum) =
    let sums =
        name
        |> Seq.groupBy id
        |> Seq.map (fun (x, xs) -> (Seq.length xs, x))
        |> Seq.sortBy (fun (n, x) -> (-1 * n, x))
        |> Seq.take 5
        |> Seq.map snd
        |> Seq.map string
        |> String.concat ""

    sums = checksum

input
|> Seq.map parseRoom
|> Seq.filter validateRoom
|> Seq.map (fun (_, sector, _) -> sector)
|> Seq.reduce (+)
|> printfn "%A"
