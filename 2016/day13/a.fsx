// vim: set et ts=4 sw=4 list :

open System

type Location = int * int
type RouteMap = Map<Location, Location>

let input = 1350
(*let input = 10*)
let start = (1, 1)
let destination = (31, 39)
(*let destination = (7, 4)*)

let isOpen (x, y) =
    let rec onBits n =
        if n = 0 then true
        else if n % 2 = 0 then onBits (n / 2) else (not (onBits (n / 2)))

    let value = (x * x) + (3 * x) + (2 * x * y) + (y) + (y * y) + input

    onBits value

let bfs start destination =
    let neighbors (x, y) =
        [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)]
        |> List.filter (fun (x, y) -> x >= 0 && y >= 0)
        |> List.filter isOpen

    let retrace map destination =
        let rec go trail cur =
            printfn "retracing %A" cur
            if cur = start then cur :: trail
            else
                match Map.tryFind cur map with
                | Some next -> go (cur :: trail) next
                | None -> trail

        go [] destination

    let rec step map visited queue =
        let current = List.head queue
        printfn "processing %A" current
        if current = destination
        then retrace map current
        else if Set.contains current visited
        then step map visited (List.tail queue)
        else
            let newPotential = neighbors current |> List.filter (fun s -> (not (Map.containsKey s map)))
            let newQueue = List.append (List.tail queue) newPotential

            let newMapEntries = newPotential |> List.map (fun s -> (s, current))
            let newMap = map |> Map.toList |> List.append newMapEntries |> Map.ofList

            step newMap (Set.add current visited) newQueue

    step (Map.empty) Set.empty [start]

(*printfn "%A" (isOpen (7, 4))*)
(*printfn "%A" (isOpen (8, 4))*)

let result = bfs start destination
printfn "%A" result
printfn "%A" ((List.length result) - 1)
