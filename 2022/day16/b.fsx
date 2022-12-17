// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

let parseInput rawInput =
    rawInput
    |> List.choose (function
        | Regex "Valve ([A-Z]+) has flow rate=(-?\d+); tunnels? leads? to valves? ([ ,A-Z]+)" [this; rate; next] ->
            Some (this, (int rate, List.ofSeq (next.Split(", "))))
        | s ->
            printfn "invalid row \"%s\"" s
            None)
    |> Map.ofList

let simplifyGraph map =
    let edges =
        map
        |> Map.toList
        |> List.collect (fun (cur, (_, nexts)) -> nexts |> List.map (fun next -> ((cur, next), 1)))
        |> Map.ofList

    let findDistance map x y =
        map |> Map.tryFind (x, y) |> Option.defaultValue 10000 // 10000 is effectively infinite for this

    let distGraph =
        let nodeList = map |> Map.keys

        [ for k in nodeList do
          for i in nodeList do
          for j in nodeList do
              (k, i, j)]
        |> List.fold (fun m (k, i, j) ->
            if findDistance m i j > findDistance m i k + findDistance m j k then
                m |> Map.add (i, j) (findDistance m i k + findDistance m j k)
            else m) edges

    let nonStuckValves = map |> Map.filter (fun _ (flow, _) -> flow > 0) |> Map.keys |> List.ofSeq |> List.append ["AA"]

    distGraph |> Map.filter (fun (f, t) _ -> f <> t && (List.contains f nonStuckValves && List.contains t nonStuckValves))

let findAllRoutes maxDistance flows distances =
    let nonStuckValves = distances |> Map.keys |> Seq.toList |> List.map fst |> Set.ofList

    let rec step paths queue =
        match queue with
        | [] ->
            // paths |> List.iter (printfn "%A")
            paths
        | (timeLeft, cumFlow, path) :: rest ->
            // printfn "%A" (timeLeft, cumFlow, path)
            let cur = List.head path

            let nextSteps = 
                Set.difference nonStuckValves (Set.ofList path)
                |> Set.filter (fun n -> distances |> Map.find (cur, n) < timeLeft)
                |> Set.toList
                |> List.map (fun n -> (timeLeft - ((Map.find (cur, n) distances) + 1), cumFlow + (flows |> Map.find n |> fst) * (timeLeft - (Map.find (cur, n) distances) - 1), n :: path))

            if List.isEmpty nextSteps then
                step ((cumFlow, path) :: paths) rest
            else
                step ((cumFlow, path) :: paths) (rest @ nextSteps)
    
    step [] [(maxDistance, 0, ["AA"])]
        

let run input =
    let edgeGraph = simplifyGraph input

    // printfn "input:"
    // input |> Map.iter (fun k v -> printfn "%A %A" k v)
    // printfn "edges:"
    // edgeGraph |> Map.iter (fun k v -> printfn "%A %A" k v)
    // printfn "-----"

    let allRoutes =
        findAllRoutes 26 input edgeGraph
        |> List.sortDescending
        |> List.map (fun (flow, path) -> (flow, Set.difference (Set.ofList path) (Set.singleton "AA")))
        // |> List.head |> fst

    printfn "all routes found: %A" (List.length allRoutes)

    // let allValves = edgeGraph |> Map.keys |> Seq.map fst |> Set.ofSeq

    // let invertedRoutes =
        // allRoutes
        // |> List.groupBy (fun (_, path) -> Set.difference allValves path)
        // |> List.map (fun (key, values) -> (key, List.sortDescending values))
        // |> Map.ofList

    // Map.iter (fun k v -> printfn "%A %A" k v) invertedRoutes

    // printfn "routes inverted"

    // printfn "allValves"
    // printfn "%A" allValves
    // printfn "allRoutes"
    // printfn "%A" allRoutes
    // printfn "invertedRoutes"
    // printfn "%A" invertedRoutes

    allRoutes 
    |> Seq.choose (fun (flow1, path1) ->
        let elephant = allRoutes |> List.tryFind (fun (_, path2) -> Set.isEmpty (Set.intersect path1 path2))
        match elephant with
        | Some (flow2, path2) ->
            printfn "found a match: %A %A %A" (flow1, path1) (flow2, path2) (flow1 + flow2)
            Some(flow1 + flow2, Set.union path1 path2)
        | None -> None)
    // |> Seq.take 100
    |> Seq.max

doProcess parseInput run rawInput
