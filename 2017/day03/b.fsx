// vim: set et ts=4 sw=4 list :

open System

let input = 312051

let squares = [((0, 0), 1)] |> Map.ofList

let generateRing n =
    let right = [-n..(n-1)] |> List.rev |> List.map (fun r -> (r, n))
    let top = [-n..(n-1)] |> List.rev |> List.map (fun c -> (-n, c))
    let left = [-(n-1)..n] |> List.map (fun r -> (r, -n))
    let bottom = [-(n-1)..n] |> List.map (fun c -> (n, c))

    List.concat [right;top;left;bottom]

let cells = Seq.collect id (Seq.initInfinite generateRing)

let getValue map (x, y) =
    match Map.tryFind (x, y) map with
    | None -> 0
    | Some z -> z

let neighbors (x, y) = [(x - 1, y - 1); (x - 1, y); (x - 1, y + 1); (x, y - 1); (x, y + 1); (x + 1, y - 1); (x + 1, y); (x + 1, y + 1)]

let computeValue map (x, y) = 
    (x, y)
    |> neighbors
    |> Seq.map (getValue map)
    |> Seq.sum

let calculate (_, map) (x, y) =
    let value = computeValue map (x, y)
    (value, Map.add (x, y) value map)

let result =
    cells
    |> Seq.scan calculate (1, squares)
    |> Seq.map fst
    |> Seq.where (fun n -> n > input)
    |> Seq.head

printfn "%A" result
