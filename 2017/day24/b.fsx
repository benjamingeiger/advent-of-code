// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readLines filePath = System.IO.File.ReadLines filePath
let input =
    readLines "input.txt"
    |> Seq.choose (function
        | Regex "(\d+)/(\d+)" [a;b] ->
            Some (int a, int b)
        | s ->
            eprintfn "Invalid adapter [%s]" s
            None)
    |> Set.ofSeq

let longestPaths start edges =

    let acceptable current edges =
        edges |> Set.filter (fun (x, y) -> x = current || y = current)

    let rec go path currentValue edges =
        let potentialFit = acceptable currentValue edges

        if Set.isEmpty potentialFit then
            (*printfn "%A" path*)
            [path]
        else
            potentialFit
            |> Set.toList
            |> List.collect (fun (x, y) ->
                go ((x, y) :: path)
                   (if y = currentValue then x else y) 
                   (Set.remove (x, y) edges))

    go [] 0 edges

let computeStrength path = path |> List.map (fun (a, b) -> a + b) |> List.sum

let bridges = longestPaths 0 input

let longestLength =
    bridges
    |> List.map List.length
    |> List.max

let longestBridge =
    bridges
    |> List.filter (fun l -> List.length l = longestLength)
    |> List.map computeStrength
    |> List.max

printfn "%A" longestBridge

