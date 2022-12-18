// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

let parseInput rawInput =
    rawInput
    |> List.choose (function
        | Regex "^(-?\d+),(-?\d+),(-?\d+)$" [x; y; z] ->
            Some (int x, int y, int z)
        | x ->
            eprintfn "misparsed: %A" x
            None)
    |> Set.ofList

let findPockets cubes =
    let cubes' = cubes |> Set.toList
    let minX = cubes' |> List.map (fun (x, _, _) -> x) |> List.min
    let maxX = cubes' |> List.map (fun (x, _, _) -> x) |> List.max
    let minY = cubes' |> List.map (fun (_, y, _) -> y) |> List.min
    let maxY = cubes' |> List.map (fun (_, y, _) -> y) |> List.max
    let minZ = cubes' |> List.map (fun (_, _, z) -> z) |> List.min
    let maxZ = cubes' |> List.map (fun (_, _, z) -> z) |> List.max

    let allCubes = 
        seq {
            for x in (minX - 1) .. (maxX + 1) do
            for y in (minY - 1) .. (maxY + 1) do
            for z in (minZ - 1) .. (maxZ + 1) do
                (x, y, z)
        }
        |> Set.ofSeq
    
    let hollowCubes = Set.difference allCubes cubes

    let neighbors (x, y, z) =
        [
            (x + 1, y, z)
            (x - 1, y, z)
            (x, y + 1, z)
            (x, y - 1, z)
            (x, y, z + 1)
            (x, y, z - 1)
        ]
        |> List.filter (fun c -> Set.contains c hollowCubes)

    let rec step counter visited = function
        | [] -> visited
        | cur :: rest ->
            if Set.contains cur visited then step (counter + 1) visited rest else

            let ns =
                cur
                |> neighbors
                |> List.filter (fun c -> Set.contains c visited |> not)

            step (counter + 1) (Set.add cur visited) (rest @ ns)

    let outerSpace = step 0 (Set.empty) [(Set.minElement hollowCubes)]

    let pockets = Set.difference allCubes (Set.union outerSpace cubes)

    pockets

let countSides cubes =
    let initialSides = Set.count cubes * 6

    let rightCovered =
        cubes
        |> Set.filter (fun (x, y, z) -> cubes |> Set.contains (x + 1, y, z))
        |> Set.count
    let leftCovered =
        cubes
        |> Set.filter (fun (x, y, z) -> cubes |> Set.contains (x - 1, y, z))
        |> Set.count
    let topCovered =
        cubes
        |> Set.filter (fun (x, y, z) -> cubes |> Set.contains (x, y + 1, z))
        |> Set.count
    let bottomCovered =
        cubes
        |> Set.filter (fun (x, y, z) -> cubes |> Set.contains (x, y - 1, z))
        |> Set.count
    let frontCovered =
        cubes
        |> Set.filter (fun (x, y, z) -> cubes |> Set.contains (x, y, z + 1))
        |> Set.count
    let backCovered =
        cubes
        |> Set.filter (fun (x, y, z) -> cubes |> Set.contains (x, y, z - 1))
        |> Set.count

    initialSides - (rightCovered + leftCovered + topCovered + bottomCovered + frontCovered + backCovered)

let run input =
    let pockets = input |> findPockets

    (countSides input) - (countSides pockets)

doProcess parseInput run rawInput
