// vim: set et ts=4 sw=4 list :

open System

type Location = int * int
type State = Location * int
type RouteMap = Map<Location, Location>

let input = 1350
let start = (1, 1)
let destination = (31, 39)

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

    let rec step visited queue =
        if queue = [] then visited
        else
            let (current, distance) = List.head queue

            if Set.contains current visited
            then step visited (List.tail queue)
            else if distance >= 50
            then step (Set.add current visited) (List.tail queue)
            else
                let newQueue =
                    neighbors current 
                    |> List.filter (fun s -> (not (Set.contains s visited)))
                    |> List.map (fun p -> (p, distance + 1))
                    |> List.append (List.tail queue)

                step (Set.add current visited) newQueue

    step Set.empty [(start, 0)]

let result = bfs start destination
printfn "%A" result
printfn "%A" (Set.count result)
