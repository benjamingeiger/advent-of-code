// vim: set et ts=4 sw=4 list :

open System


// borrowed from https://stackoverflow.com/questions/1526046/f-permutations
let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)


let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let parseCell (cells, waypoints) ((r, c), cell) =
    match cell with
    | '#' -> (cells, waypoints)
    | '.' -> (Set.add (r, c) cells, waypoints)
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
        (Set.add (r, c) cells, Map.add ((int cell) - (int '0')) (r, c) waypoints)
    | c ->
        printfn "Unexpected char [%A]" c
        (cells, waypoints)

let (cells, waypoints) =
    input
    |> Seq.map Seq.indexed
    |> Seq.indexed
    |> Seq.map (fun (r, cs) -> Seq.map (fun c -> (r, c)) cs)
    |> Seq.concat
    |> Seq.map (fun (r, (c, cell)) -> ((r, c), cell))
    |> Seq.fold parseCell (Set.empty, Map.empty)

let routes cells waypoints =
    let waypointReverse = waypoints |> Map.toList |> List.map (fun (a, b) -> (b, a)) |> Map.ofList

    let retrace map destination =
        let rec go trail cur =
            match Map.tryFind cur map with
            | Some next -> 
                if cur = next then (List.length trail) else go (cur :: trail) next
            | None -> List.length trail

        go [] destination

    let retraceAll map =
        waypoints
        |> Map.toSeq
        |> Seq.map (fun (k, v) -> (k, retrace map v))

    let neighbors (r, c) =
        [(r - 1, c); (r + 1, c); (r, c - 1); (r, c + 1)]
        |> List.filter (fun p -> Set.contains p cells)

    let updateMap map newElements =
        map |> Map.toList |> List.append newElements |> Map.ofList

    let rec search predecessors visited = function
        | [] -> predecessors
        | cur :: frontier ->
            if Set.contains cur visited
            then search predecessors visited frontier
            else
                let newPotentials =
                    neighbors cur
                    |> List.filter (fun c -> (not (Map.containsKey c predecessors)))

                let newFrontier = List.append frontier newPotentials

                let newPredecessors =
                    updateMap predecessors (newPotentials |> List.map (fun c -> (c, cur)))

                search newPredecessors (Set.add cur visited) newFrontier

    let waypointRoutes =
        waypoints
        |> Map.toSeq
        |> Seq.map (fun (k, (r, c)) -> (k, (r, c), search (Map.add (r, c) (r, c) Map.empty) Set.empty [(r, c)]))
        |> Seq.map (fun (k, _, map) -> (k, retraceAll map))
        |> Seq.map (fun (x, ds) -> Seq.map (fun (y, d) -> ((x, y), d)) ds)
        |> Seq.concat
        |> Seq.filter (fun ((x, y), d) -> x <> y)
        |> Map.ofSeq

    waypointRoutes

let allRoutes = routes cells waypoints

let findHamiltonPath (routes : Map<int * int, int>) initial =
    // brute force is acceptable since this is small
    
    let sumRoute vs =
        vs
        |> List.pairwise
        |> List.map (fun (x, y) -> Map.find (x, y) routes)
        |> List.sum
    
    let vertices =
        routes
        |> Map.toSeq
        |> Seq.map (fun (k, v) -> fst k)
        |> Seq.distinct
        |> Seq.except [initial]
        |> Seq.toList

    vertices
    |> permute
    |> List.map (fun p -> List.concat [[initial]; p; [initial]] |> sumRoute)
    |> List.min

let path = findHamiltonPath allRoutes 0
printfn "%A" path
