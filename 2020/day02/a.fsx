// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let splitIntoParts (s : string) =
    let parts = s.Split ':'
    let subparts = parts.[0].Split ' '
    let range = subparts.[0].Split '-' |> Array.map int
    let target = subparts.[1].[0]

    range.[0], range.[1], target, parts.[1]

let isValidPassword (low, high, target, password : string) =
    let rec isValidPassword' chars count =
        match chars with
        | [] -> count >= low && count <= high
        | h :: t when h = target -> isValidPassword' t (count + 1)
        | h :: t -> isValidPassword' t count

    isValidPassword' (Seq.toList password) 0

let result = input 
                |> Seq.map splitIntoParts
                |> Seq.filter isValidPassword
                |> Seq.length

printfn "%A" result


