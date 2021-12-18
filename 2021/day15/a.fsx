// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> List.ofSeq


let parseGrid input =
    input
    |> indexCharacters (fun c -> int c - int '0')
    |> Map.ofSeq

let dijkstra (nodes : Map<int * int, int>) (start : int * int) (goal : (int * int)) =
    let INFINITY = System.Int32.MaxValue

    let neighbors (r, c) =
        [(r + 1, c); (r - 1, c); (r, c + 1); (r, c - 1)]
        |> List.filter (fun p -> Map.containsKey p nodes)

    let distance = snd >> fst
    let init ((v : int * int), _) = (v, (INFINITY, v))

    let nodeMap =
        nodes
        |> Map.toSeq
        |> Seq.map init
        |> Map.ofSeq
        |> Map.add start (0, start)

    printfn "%A" nodeMap

    let allInfinity map = map |> Map.forall (fun _ (d, _) -> d = INFINITY)

    let closest map = map |> Map.toSeq |> Seq.minBy distance

    let rec step processed  (unprocessed : Map<int * int, int * (int * int)>) =
        if Map.isEmpty unprocessed then processed
        elif allInfinity unprocessed then processed
        else
            let (n, (d, p)) = closest unprocessed

            if n = goal then Map.add n (d, p) processed
            else

                printfn "processing %A (%A, %A)" n d p

                let updatedUnprocessed =
                    unprocessed
                    |> Map.remove n
                    |> Map.map (fun n' (d', p') ->
                        let targetValue = Map.find n' nodes
                        if n |> neighbors |> List.contains n' then
                            if d' + targetValue < d' then (d + targetValue, n) else (d', p')
                        else (d', p'))

                step (Map.add n (d, p) processed) updatedUnprocessed

    step Map.empty nodeMap

let result =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let nodes = parseGrid input

    let returnValue = dijkstra nodes (0, 0) (10, 10) |> Map.toSeq |> Seq.maxBy fst

    stopwatch.Stop()
    printfn "Elapsed milliseconds: %f" stopwatch.Elapsed.TotalMilliseconds

    returnValue

printfn "%A" result
