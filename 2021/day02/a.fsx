// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt"

type Direction =
    | Up of int
    | Down of int
    | Forward of int

let matchDirection = function
    | Regex "forward (\d+)" [x] -> Forward (Int32.Parse x)
    | Regex "up (\d+)" [x] -> Up (Int32.Parse x)
    | Regex "down (\d+)" [x] -> Down (Int32.Parse x)
    | _ -> failwith "lolwut"

let execute (depth, pos) = function
    | Forward x -> (depth, pos + x)
    | Up x -> (depth - x, pos)
    | Down x -> (depth + x, pos)

let result =
    input
    |> Seq.map matchDirection
    |> Seq.fold execute (0, 0)
    |> fun (x, y) -> x * y

printfn "%A" result
