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

let execute (aim, depth, pos) = function
    | Forward x -> (aim, depth + (x * aim), pos + x)
    | Up x -> (aim - x, depth, pos)
    | Down x -> (aim + x, depth, pos)

let result =
    input
    |> Seq.map matchDirection
    |> Seq.fold execute (0, 0, 0)
    |> fun (_, x, y) -> x * y

printfn "%A" result
