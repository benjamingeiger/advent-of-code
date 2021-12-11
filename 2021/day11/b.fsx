// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> indexCharacters (fun x -> int x - int '0') |> Map.ofSeq

let neighbors (r, c) =
    [
        (r - 1, c - 1); (r - 1, c); (r - 1, c + 1);
        (r,     c - 1);             (r,     c + 1);
        (r + 1, c - 1); (r + 1, c); (r + 1, c + 1)
    ]

let flash map =
    let rec go alreadyFlashed map' =
        let aboutToFlash =
            map'
            |> Map.filter (fun _ v -> v > 9)
            |> Map.toSeq |> Seq.map fst |> Set.ofSeq
            |> (fun s -> Set.difference s alreadyFlashed)

        let toIncrease =
            aboutToFlash
            |> Set.toSeq
            |> Seq.collect neighbors
            |> Seq.filter (fun (r, c) -> not (Set.contains (r, c) (Set.union alreadyFlashed aboutToFlash)))

        let newMap =
            Seq.fold (fun acc elem -> acc |> Map.change elem (Option.bind (fun v -> Some (v + 1)))) map' toIncrease

        if Set.isEmpty aboutToFlash then (map', alreadyFlashed) else go (Set.union alreadyFlashed aboutToFlash) newMap

    go Set.empty map

let step map =
    map
    |> Map.map (fun _ v -> v + 1)
    |> flash
    |> fun (map', flashed) -> (Map.map (fun k v -> if Set.contains k flashed then 0 else v) map', flashed)

let doSteps map =
    let mapSize = map |> Map.toSeq |> Seq.length

    let rec go map' stepCount =
        let (newMap, flashed) = step map'

        if Set.count flashed = mapSize then stepCount + 1 else go newMap (stepCount + 1)

    go map 0

printfn "%A" (doSteps input)
