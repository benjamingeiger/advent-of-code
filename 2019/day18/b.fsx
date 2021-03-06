// vim: set et ts=4 sw=4 list :

open System

type Cell =
    | Open
    | Door of char
    | Key of char
    | Start

let readLines filePath = System.IO.File.ReadLines filePath
let input =
    readLines "input.txt"
    |> Seq.map Seq.indexed
    |> Seq.indexed
    |> Seq.map (fun (r, cs) -> cs |> Seq.map (fun (c, x) -> ((r, c), x)))
    |> Seq.collect id
    |> Seq.choose (fun ((r, c), x) ->
        match x with
        (*| '#' -> Some ((r, c), Wall)*)
        | '#' -> None
        | '.' -> Some ((r, c), Open)
        | x' when Char.IsUpper(x') -> Some ((r, c), Door (Char.ToLower(x')))
        | x' when Char.IsLower(x') -> Some ((r, c), Key (Char.ToLower(x')))
        | '@' -> Some ((r, c), Start)
        | _ ->
            eprintfn "Invalid character [%A]" x
            None)
    |> Map.ofSeq

type Queue<'t> = ('t list * 't list)

let push x (front, back) = (front, x :: back)

let pushMany xs (front, back) = (front, (Seq.rev xs |> List.ofSeq) @ back)

let rec pop = function
    | ([], []) -> None
    | ([], back) -> pop (List.rev back, [])
    | (x::xs, back) -> Some (x, (xs, back))

// TODO:

// Go through the output of shortestPaths, removing any doors that don't have
// corresponding keys. The idea being, we don't care which order the keys are
// collected between chambers; they'll automatically sit and wait.
//
// Yeah, it wouldn't be acceptable for real world code, since there'd be no
// guarantee that the keys could be collected at all, but in this limited
// scenario it'll suffice.

let shortestPaths maze start =
    let neighbors (r, c) =
        [(r - 1, c); (r + 1, c); (r, c - 1); (r, c + 1)]
        |> Seq.choose (fun (r', c') ->
            match Map.tryFind (r', c') maze with
            | None -> None
            | Some _ -> Some (r', c'))

    let rec go targets visited queue =
        match pop queue with
        | None -> targets
        | Some ((pos, distance, doors) , queue') ->
            let cur = Map.find pos maze
            let distance' = distance + 1
            let doors' = match cur with | Door d -> d :: doors | _ -> doors

            let queue'' = 
                neighbors pos
                |> Seq.filter (fun (r', c') -> (Set.contains (r', c') visited) |> not)
                |> Seq.map (fun pos' -> (pos', distance', doors'))
                |> fun xs -> pushMany xs queue'

            match cur with
            | Key k -> go (Map.add k (pos, distance, doors) targets) (Set.add pos visited) queue''
            | _ -> go targets (Set.add pos visited) queue''

    go Map.empty Set.empty ([(start, 0, [])], [])

let starts =
    input 
    |> Map.filter (fun _ v -> v = Start)
    |> Map.toList
    |> List.map fst

let run startPos =

    let keysFromStart' = shortestPaths input startPos

    let reachableKeys =
        keysFromStart'
        |> Map.toList
        |> List.map fst
        |> Set.ofList

    let keysFromStart =
        keysFromStart'
        |> Map.map (fun _ (pos, d, doors) -> (pos, d, List.filter (fun c -> Set.contains c reachableKeys) doors))

    let generateDirections source result =
        result
        |> Map.toList
        |> List.map (fun (k, (_, d, keys)) -> ((source, k), (d, Set.intersect (Set.ofList keys) reachableKeys)))

    let keyDistances =
        keysFromStart
        |> Map.toList
        |> List.collect (fun (k, (pos, _, _)) -> 
            shortestPaths input pos
            |> generateDirections k)

    let distances = (keysFromStart |> generateDirections '@') @ keyDistances

    let potentialNextEdges distances (distance, keys, cur) =
        let isAdjacent (f, t) _ = f = (List.head cur) && not (Set.contains t keys)
        let isAccessible (_, _) (_, doors) = Set.isSubset doors keys

        distances
        |> Map.filter (fun k v -> (isAdjacent k v) && (isAccessible k v))
        |> Map.toSeq
        |> Set.ofSeq
        |> Set.map (fun ((_, t), (d, doors)) -> (distance + d, Set.add t keys, t :: cur))

    let dijkstra distances =
        let allKeys =
            distances
            |> Map.toList
            |> List.map fst
            |> List.map snd
            |> Set.ofList
            |> Set.add '@'

        eprintfn "%A" allKeys

        let rec go s q =
            let (distance, keys, cur) = Set.minElement q
            let q' = Set.remove (distance, keys, cur) q

            if keys = allKeys then
                distance
            elif Set.contains (keys, List.head cur) s then
                if Set.isEmpty q then
                    distance
                else go s q'
            else
                let s' = Set.add (keys, List.head cur) s

                let neighbors = potentialNextEdges distances (distance, keys, cur)

                go s' (Set.union q' neighbors)

        go (Set.empty) (Set.singleton (0, Set.singleton '@', ['@']))

    dijkstra (Map.ofList distances)

input
|> Map.toSeq
|> Seq.choose (fun (pos, cell) -> if cell = Start then Some pos else None)
|> Seq.map run
|> Seq.sum
|> printfn "%A"

