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
        | Regex "Step (.) must be finished before step (.) can begin." [before; after] -> Some (before, after)
        | s ->
            printfn "Invalid instruction %A" s
            None)
    |> Seq.toList

printfn "%A" input

let toposort edges =
    let findStarts es =
        let fromVertices = es |> Seq.map fst |> Set.ofSeq
        let toVertices = es |> Seq.map snd |> Set.ofSeq

        Set.difference fromVertices toVertices |> Set.toList |> List.sort

    let removeOutgoingEdges v es =
        List.filter (fun (a, b) -> a <> v) es

    let rec go visited es =
        match es with
        | [] -> visited
        | _ ->
            match findStarts es with
            | [] -> failwith "whoops, found a cycle"
            | x :: xs -> go (x :: visited) (removeOutgoingEdges x es)

    let vertices =
        Seq.append (edges |> Seq.map fst) (edges |> Seq.map snd) |> Seq.distinct

    // This is a clunky solution but it works.
    // A vertex with out-degree of 0 doesn't get included in the output, so
    // we add an edge from every existing vertex to a new dummy vertex.
    let augmentedEdges =
        vertices
        |> Seq.map (fun x -> (x, "nope"))
        |> Seq.append edges
        |> Seq.toList

    go [] augmentedEdges |> List.rev

let result = toposort input |> String.concat ""

printfn "%A" result
