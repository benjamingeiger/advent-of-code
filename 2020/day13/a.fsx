// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic 

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let timestamp = Seq.head input |> int
let routeInput = Seq.head (Seq.tail input)
let routes = routeInput.Split "," |> Array.filter ((<>) "x") |> Array.map int

let wait =
    routes
    |> Seq.map (fun route -> if timestamp % route = 0 then (0, route) else (route - (timestamp % route), route))
    |> Seq.minBy fst
    |> (fun (a, b) -> a * b)

printfn "%A" wait
