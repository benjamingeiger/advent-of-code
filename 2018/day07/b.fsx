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

let elfensort dependencies =

    let augment edges =
        edges
        |> Seq.collect (fun (u, v) -> [u; v])
        |> Seq.distinct
        |> Seq.map (fun x -> (x, "nope"))
        |> Seq.append edges
        |> Seq.toList

    let jobTime (s : string) = 60 + (int s.[0] - int 'A') + 1

    let findJobs deps =
        let fromVertices = deps |> Seq.map fst |> Set.ofSeq
        let toVertices = deps |> Seq.map snd |> Set.ofSeq

        Set.difference fromVertices toVertices |> Set.toList |> List.sort

    let removeOutgoingEdges deps j =
        List.filter (fun (a, b) -> a <> j) deps

    let rec step completed working deps time =
        let (working', newlyCompleted) = working |> List.partition (fun (t, _) -> t > time)
        let completed' = List.append newlyCompleted completed
        let deps' = newlyCompleted |> List.map snd |> List.fold removeOutgoingEdges deps

        let currentJobCount = List.length working'
        let potentialJobs =
            findJobs deps'
            |> List.filter (fun j -> not ((List.exists (fun (_, j') -> j = j')) (List.append completed' working')))
        let working'' =
            potentialJobs
            |> List.truncate (5 - currentJobCount)
            |> List.map (fun j -> (time + jobTime j, j))
            |> List.append working'

        if working'' = []
        then completed'
        else
            let time' = working'' |> List.map fst |> List.min

            step completed' working'' deps' time'

    step [] [] (augment dependencies) 0

printfn "%A" (elfensort input |> List.map fst |> List.max)

