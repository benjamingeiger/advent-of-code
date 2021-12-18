// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> List.ofSeq


let parseGrid input =
    input
    |> indexCharacters (fun c -> int c - int '0')
    |> Map.ofSeq

let duplicateGrid grid =
    let (maxR, maxC) = grid |> Map.toSeq |> Seq.maxBy fst |> fun ((r, c), _) -> (r + 1, c + 1)

    let rollover n d = if d + n > 9 then (d + n - 9) else d + n

    grid
    |> Map.toList
    |> Seq.collect (fun ((r, c), d) ->
        ([0..4] |> List.map (fun n -> ((r, c + (maxC * n)), rollover n d))))
    |> Seq.collect (fun ((r, c), d) ->
        ([0..4] |> List.map (fun n -> ((r + (maxR * n), c), rollover n d))))
    |> Map.ofSeq

let astar (nodes : Map<int * int, int>) (start : int * int) (goal : (int * int)) =
    let INFINITY = System.Int32.MaxValue / 2

    let neighbors (r, c) =
        [(r + 1, c); (r - 1, c); (r, c + 1); (r, c - 1)]
        |> List.filter (fun p -> Map.containsKey p nodes)

    let init (v : int * int) = (v, (INFINITY, v))

    let nodeMap =
        Map.empty
        |> Map.add start (0, start)

    printfn "%A" nodeMap

    let allInfinity map = map |> Map.forall (fun _ (d, _) -> d = INFINITY)

    let closest map =

        printfn "%A" goal

        map
        |> Map.toSeq
        |> Seq.minBy (fun ((r, c), (d, _)) -> d + (fst goal - r) + (snd goal - c))

    let rec step processed  (unprocessed : Map<int * int, int * (int * int)>) =
        printfn "%A" unprocessed
        if Map.isEmpty unprocessed then processed
        elif allInfinity unprocessed then processed
        else
            let (n, (d, p)) = closest unprocessed

            if n = goal then Map.add n (d, p) processed
            else

                printfn "processing %A (%A, %A)" n d p

                let expandedUnprocessed =
                    neighbors n
                    |> Seq.filter (fun n' -> Map.containsKey n' unprocessed |> not)
                    |> Seq.filter (fun n' -> Map.containsKey n' processed |> not)
                    |> Seq.map init

                let updatedUnprocessed =
                    unprocessed
                    |> fun m -> (expandedUnprocessed |> Seq.fold (fun acc (n', (d', p')) -> Map.add n' (d', p') acc) m)
                    |> Map.remove n
                    |> Map.map (fun n' (d', p') ->
                        let targetValue = Map.find n' nodes
                        if n |> neighbors |> List.contains n' then
                            if d + targetValue < d' then
                                printfn "Updating %A to (%A, %A)" n' (d + targetValue) n
                                (d + targetValue, n)
                            else
                                printfn "Not updating: %A to (%A, %A) (targetValue = %A)" n' d' p' targetValue
                                (d', p')
                        else (d', p'))

                printfn "%A" (allInfinity updatedUnprocessed)

                step (Map.add n (d, p) processed) updatedUnprocessed

    step Map.empty nodeMap

let result =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let nodes = parseGrid input |> duplicateGrid
    let target = nodes |> Map.toSeq |> Seq.map fst |> Seq.max

    let returnValue = astar nodes (0, 0) target |> Map.toSeq |> Seq.maxBy fst

    stopwatch.Stop()
    printfn "Elapsed milliseconds: %f" stopwatch.Elapsed.TotalMilliseconds

    returnValue

printfn "%A" result
