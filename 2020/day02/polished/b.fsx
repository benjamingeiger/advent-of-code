// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"


// Parse the input line.
let extractValues (s : string) =
    let pattern = @"^(\d+)-(\d+) (.): (.*)$"
    let regex = Regex pattern

    let m = regex.Match s

    let lower = int m.Groups.[1].Value
    let upper = int m.Groups.[2].Value
    let target = m.Groups.[3].Value.[0]
    let password = m.Groups.[4].Value.Trim()

    lower, upper, target, password


// Is this password valid, given the request?
let isValidPassword (low, high, target, password : string) =
    (password.[low - 1] = target) <> (password.[high - 1] = target)


let result = input 
                |> Seq.map extractValues
                |> Seq.filter isValidPassword
                |> Seq.length

printfn "%A" result


