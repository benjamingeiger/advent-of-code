// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic

let depth = 3198I
let targetx = 12
let targety = 757

(*let depth = 510I*)
(*let targetx = 10*)
(*let targety = 10*)

let memo = new Dictionary<int * int, bigint>()

let rec geologicIndex (x, y) =
    let go' () =
        match (x, y) with
        | (0, 0) -> 0I
        | (x', y') when (x', y') = (targetx, targety) -> 0I
        | (_, 0) -> ((bigint x) * 16807I) % 20183I
        | (0, _) -> ((bigint y) * 48271I) % 20183I
        | (_, _) -> (erosionLevel (x - 1, y) * erosionLevel (x, y - 1)) % 20183I

    match memo.TryGetValue((x, y)) with
    | true, v -> v
    | false, _ ->
        let v = go' ()
        memo.Add((x, y), v)
        v

and erosionLevel (x, y) =
    let gi = geologicIndex (x, y)
    (gi + depth) % 20183I

type RegionType = Rocky | Wet | Narrow

let regionType (x, y) = 
    match erosionLevel (x, y) % 3I with
    | x when x = 0I -> Rocky
    | x when x = 1I -> Wet
    | x when x = 2I -> Narrow
    | x -> failwithf "Invalid region type %A" x

type Tool = ClimbingGear | Torch | Neither

let compatibleWithTool tool region =
    match (region, tool) with
    | (Rocky, Neither) -> false
    | (Wet, Torch) -> false
    | (Narrow, ClimbingGear) -> false
    | (_, _) -> true

let switchTool tool region =
    match (region, tool) with
    | (Rocky, ClimbingGear) -> Torch
    | (Rocky, Torch) -> ClimbingGear
    | (Wet, ClimbingGear) -> Neither
    | (Wet, Neither) -> ClimbingGear
    | (Narrow, Torch) -> Neither
    | (Narrow, Neither) -> Torch
    | _ -> failwithf "Invalid tool %A for region %A" tool region

let route (sourcex, sourcey) (targetx, targety) =
    let neighbors (distance, ((x, y), tool)) =
        let currentRegionType = regionType (x, y)

        [                (x - 1, y);
         (x,     y - 1);             (x,     y + 1);
                         (x + 1, y);               ]
        |> List.filter (fun (x', y') -> x' >= 0 && y' >= 0)
        |> List.filter (fun loc -> loc |> regionType |> compatibleWithTool tool)
        |> List.map (fun (x', y') -> (distance + 1, ((x', y'), tool)))
        |> fun xs -> (distance + 7, ((x, y), (switchTool tool currentRegionType))) :: xs

    let heuristic ((x, y), _) = abs(targetx - x) + abs(targety - y)

    let rec step visited fringe =
        let (dh, (d, ((x, y), t))) = Set.minElement fringe

        if ((x, y), t) = ((targetx, targety), Torch) then
            // We're using an "admissible" heuristic, so the first time we reach
            // the target, that's going to be the shortest path.
            d
        elif Set.contains ((x, y), t) visited then
            step visited (Set.remove (dh, (d, ((x, y), t))) fringe)
        else
            let potentialNeighbors =
                neighbors (d, ((x, y), t))
                |> List.filter (fun (_, ((x, y), t)) -> (Set.contains ((x, y), t) visited) |> not)
                |> List.map (fun (d, ((x, y), t)) -> (d + (heuristic ((x, y), t)), (d, ((x, y), t))))
                |> Set.ofList

            let newFringe =
                fringe
                |> Set.remove (dh, (d, ((x, y), t)))
                |> Set.union potentialNeighbors

            step (Set.add ((x, y), t) visited) newFringe

    step Set.empty (Set.singleton (0, (0, ((sourcex, sourcey), Torch))))

let result = route (0, 0) (targetx, targety)

printfn "%A" result
