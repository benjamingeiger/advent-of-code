// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let roomPattern = @"([A-Za-z-]*)-(\d+)\[([A-Za-z]+)\]"
let roomRegex = new Regex(roomPattern)

let parseRoom s =
    let m = roomRegex.Match s

    let name = m.Groups.[1].Value
    let number = m.Groups.[2].Value |> int
    let checksum = m.Groups.[3].Value

    (name, number, checksum)

let validateRoom (name, _, checksum) =
    let sums =
        name
        |> Seq.filter ((<>) '-')
        |> Seq.groupBy id
        |> Seq.map (fun (x, xs) -> (Seq.length xs, x))
        |> Seq.sortBy (fun (n, x) -> (-1 * n, x))
        |> Seq.take 5
        |> Seq.map snd
        |> Seq.map string
        |> String.concat ""

    sums = checksum

let decryptRoom (name, sector, _) =
    let decryptCharacter c =
        let charIndex = (int c) - (int 'a')
        let newIndex = (charIndex + sector) % 26
        let newChar = char (newIndex + (int 'a'))

        if c = '-' then c else newChar

    let newName = Seq.map decryptCharacter name |> Seq.map string |> String.concat ""
    (newName, sector)

input
|> Seq.map parseRoom
|> Seq.filter validateRoom
|> Seq.map decryptRoom
|> Seq.filter (fun ((name : string), sector) -> name.Contains("north"))
|> Seq.toList
|> printfn "%A"
