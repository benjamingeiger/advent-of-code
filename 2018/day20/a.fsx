// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.head |> List.ofSeq

type Direction = North | South | East | West

let createEdge dir (r, c) =
    match dir with
    | North -> (r, c), (r - 1, c)
    | South -> (r, c), (r + 1, c)
    | East -> (r, c), (r, c + 1)
    | West -> (r, c), (r, c - 1)

let createEdges ps dir = Set.map (createEdge dir) ps

let parseInput input =
    let rec go ps edges stack currentBranch = function
        | '(' :: cs ->
            go ps edges ((ps, currentBranch) :: stack) Set.empty cs

        | '|' :: cs ->
            match stack with
            | (ps', currentBranch') :: _ ->
                go ps' edges stack (Set.union currentBranch ps) cs
            | _ -> failwith "wtf"

        | ')' :: cs ->
            match stack with
            | (ps', currentBranch') :: stack' ->
                go (Set.union currentBranch ps) edges stack' currentBranch' cs
            | _ -> failwith "wtf"

        | 'N' :: cs ->
            let newEdges = createEdges ps North
            let newPs = Set.map snd newEdges

            go newPs (Set.union edges newEdges) stack currentBranch cs

        | 'E' :: cs ->
            let newEdges = createEdges ps East
            let newPs = Set.map snd newEdges

            go newPs (Set.union edges newEdges) stack currentBranch cs

        | 'S' :: cs ->
            let newEdges = createEdges ps South
            let newPs = Set.map snd newEdges

            go newPs (Set.union edges newEdges) stack currentBranch cs

        | 'W' :: cs ->
            let newEdges = createEdges ps West
            let newPs = Set.map snd newEdges

            go newPs (Set.union edges newEdges) stack currentBranch cs

        | '^' :: cs | '$' :: cs ->
            go ps edges stack currentBranch cs

        | c :: cs ->
            printfn "invalid character [%A]" c
            go ps edges stack currentBranch cs

        | [] -> edges

    go (Set.singleton (0, 0)) Set.empty [] Set.empty input

let edges' = parseInput input
let edges = edges' |> Set.map (fun (a, b) -> (b, a)) |> Set.union edges'

let bfs edges start =
    let potentialNeighbors pos =
        edges
        |> Set.filter (fun (f, t) -> f = pos)
        |> Set.toList
        |> List.map snd

    let rec go visited = function
        | [] -> visited
        | (cur, dist) :: queue ->
            match Map.tryFind cur visited with
            | Some _ -> go visited queue
            | None ->
                let visited' = Map.add cur dist visited
                let queue' = queue @ (List.map (fun x -> (x, dist + 1)) (potentialNeighbors cur))

                go visited' queue'

    go Map.empty [start]

let rooms = bfs edges ((0, 0), 0)

rooms
|> Map.toList
|> List.map snd
|> List.max
|> printfn "%A"

