// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt"

let parseLine (s : string) =
    let parts = s.Split(" | ")
    let patterns = parts.[0].Split(" ") |> Seq.map Set.ofSeq |> List.ofSeq
    let outputs = parts.[1].Split(" ") |> Seq.map Set.ofSeq |> List.ofSeq

    (patterns, outputs)

let solve (patterns, outputs) =
    let allPatterns = patterns @ outputs |> List.distinct |> List.groupBy (Set.count) |> Map.ofSeq

    let find1 map = 
        if (Map.containsKey 2 allPatterns) then Map.add 1 (Map.find 2 allPatterns |> List.exactlyOne) map else map

    let find7 map =
        if (Map.containsKey 3 allPatterns) then Map.add 7 (Map.find 3 allPatterns |> List.exactlyOne) map else map

    let find4 map =
        if (Map.containsKey 4 allPatterns) then Map.add 4 (Map.find 4 allPatterns |> List.exactlyOne) map else map

    let find8 map =
        if (Map.containsKey 7 allPatterns) then Map.add 8 (Map.find 7 allPatterns |> List.exactlyOne) map else map

    let find3 map =
        if not (Map.containsKey 5 allPatterns) then map
        else
            let fiveSegments = Map.find 5 allPatterns

            let matchGoal = Map.find (Map.findKey (fun k _ -> k = 1 || k = 7) map) map

            let threes = List.filter (Set.isSubset matchGoal) fiveSegments

            if List.length threes = 1 then Map.add 3 (List.exactlyOne threes) map else map

    let find6 map =
        if not (Map.containsKey 6 allPatterns) then map
        else
            let sixSegments = Map.find 6 allPatterns

            let matchGoal = Map.find (Map.findKey (fun k _ -> k = 1 || k = 7) map) map

            let sixes = List.filter (Set.isSubset matchGoal >> not) sixSegments

            if List.length sixes = 1 then Map.add 6 (List.exactlyOne sixes) map else map

    let find5 map =
        if not (Map.containsKey 5 allPatterns) then map
        else
            let fiveSegments = Map.find 5 allPatterns |> List.filter (fun s -> not (Seq.contains s (map |> mapValues)))

            if not (Map.containsKey 6 map) then map
            else
                let matchGoal = Map.find 6 map

                let fives = List.filter (fun s -> Set.isSubset s matchGoal) fiveSegments

                if List.length fives = 1 then Map.add 5 (List.exactlyOne fives) map else map

    let find9 map =
        if not (Map.containsKey 6 allPatterns) then map
        else
            let sixSegments = Map.find 6 allPatterns |> List.filter (fun s -> not (Seq.contains s (map |> mapValues)))

            if not (Map.containsKey 3 map) then map
            else
                let matchGoal = Map.find 3 map

                let nines = List.filter (Set.isSubset matchGoal) sixSegments

                if List.length nines = 1 then Map.add 9 (List.exactlyOne nines) map else map

    let find2 map =
        if not (Map.containsKey 5 allPatterns) then map
        else
            let fiveSegments = Map.find 5 allPatterns |> List.filter (fun s -> not (Seq.contains s (map |> mapValues)))

            if List.length fiveSegments = 1 then Map.add 2 (List.exactlyOne fiveSegments) map else map

    let find0 map =
        if not (Map.containsKey 6 allPatterns) then map
        else
            let sixSegments = Map.find 6 allPatterns |> List.filter (fun s -> not (Seq.contains s (map |> mapValues)))

            if List.length sixSegments = 1 then Map.add 0 (List.exactlyOne sixSegments) map else map

    let mapping =
        Map.empty
        |> find1
        |> find7
        |> find4
        |> find8
        |> find3
        |> find6
        |> find5
        |> find9
        |> find2
        |> find0
        |> mapInvert

    outputs |> Seq.map (fun o -> Map.find o mapping)

printfn "%A" (input |> Seq.map (parseLine >> solve >> consolidateDigits) |> Seq.sum)
