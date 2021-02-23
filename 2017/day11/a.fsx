// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.head |> fun (s : string) -> s.Split(",") |> Array.toList

let step (r, c) = function
    | "n" -> (r + 1, c)
    | "s" -> (r - 1, c)
    | "nw" -> (r, c - 1)
    | "se" -> (r, c + 1)
    | "ne" -> (r + 1, c + 1)
    | "sw" -> (r - 1, c - 1)
    | s ->
        printfn "invalid direction %A" s
        (r, c)

let finalPos = List.fold step (0, 0) input

let rec route (r, c) =
    printfn "%A" (r, c)
    if (r, c) = (0, 0) then 0
    elif c = 0 then abs r
    elif r = 0 then abs c
    elif r > 0 && c > 0 then
        let steps = min r c
        steps + route (r - steps, c - steps)
    elif r < 0 && c < 0 then
        let steps = min (abs r) (abs c)
        steps + route (r + steps, c + steps)
    else r + c

printfn "%A" (route finalPos)
