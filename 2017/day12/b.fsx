// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parseLine = function
    | Regex "^(\d+) <-> (.*)$" [srcString; dstString] ->
        let src = int srcString
        let dsts = dstString |> fun (s : string) -> s.Split(", ")

        dsts |> Array.map (fun dst -> (src, int dst)) |> Array.toList
    | s ->
        printfn "Invalid rule [%A]" s
        []

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.toList |> List.collect parseLine

let connectedComponent edges start =
    let neighbors node =
        edges
        |> List.choose (fun (f, t) -> if f = node then Some (t) else None)

    let rec go visited = function
        | [] -> visited
        | cur :: rest ->
            if Set.contains cur visited
            then go visited rest
            else go (Set.add cur visited) (List.append (neighbors cur) rest)

    go Set.empty [start]

let countComponents edges =
    let rec go components nodes =
        if Set.isEmpty nodes then components
        else
            let cur = Set.minElement nodes

            let newComponent = connectedComponent edges cur

            go (Set.add newComponent components) (Set.difference nodes newComponent)

    go Set.empty (edges |> List.map fst |> Set.ofList)

let result = countComponents input

printfn "%A" result
printfn "%A" (Set.count result)
