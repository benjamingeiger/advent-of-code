// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let hypernetAbbaPattern = @"\[[^]]*([a-z])(?!\1)([a-z])\2\1[^]]*\]"
let outsideAbbaPattern = @"([a-z])(?!\1)([a-z])\2\1"

let isTLS s =
    let isHypernet = Regex.IsMatch(s, hypernetAbbaPattern)
    let isAbba = Regex.IsMatch(s, outsideAbbaPattern)

    isAbba && (not isHypernet)

input
|> Seq.filter isTLS
|> Seq.length
|> printfn "%A"
