// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input =
    readLines "input.txt"
    |> indexCharacters (fun x -> int x - int '0')
    |> Map.ofSeq

let neighbors (r, c) = [ (r + 1, c); (r - 1, c); (r, c + 1); (r, c - 1) ]

let lowPoints map =
    map
    |> Map.toSeq
    |> Seq.filter (fun ((r, c), d) ->
        neighbors (r, c)
        |> Seq.choose (fun (r', c') -> Map.tryFind (r', c') map)
        |> Seq.map (fun d' -> d' > d)
        |> Seq.fold (&&) true)

let flood map (r, c) =
    let rec step basin = function
        | [] -> basin
        | pos :: queue ->
            if Set.contains pos basin then
                step basin queue
            else
                let potentialNeighbors =
                    pos
                    |> neighbors
                    |> Seq.filter (fun pos' -> not ((Set.contains pos' basin) || (List.contains pos' queue)))
                    |> Seq.filter (fun pos' ->
                        match Map.tryFind pos' map with
                        | None -> false
                        | Some x -> x < 9)
                    |> List.ofSeq

                step (Set.add pos basin) (queue @ potentialNeighbors)

    step (Set.empty) [(r, c)]

let result =
    input
    |> lowPoints
    |> Seq.map (fst >> (flood input) >> Seq.length)
    |> Seq.sortByDescending id
    |> Seq.take 3
    |> Seq.fold ( * ) 1

printfn "%A" result
