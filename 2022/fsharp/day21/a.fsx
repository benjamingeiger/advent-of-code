// vim: set et ts=4 sw=4 list :

open System
open Checked

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

type Monkey =
    | Raw of bigint
    | Plus of string * string
    | Minus of string * string
    | Times of string * string
    | Divide of string * string

let parseInput rawInput =
    rawInput
    |> List.choose (function
        | Regex "(.*): (-?\d+)" [name; value] -> Some ((name, Raw (bigint (int value))))
        | Regex "(.*): ([^ ]+) \+ ([^ ]+)" [name; first; second] -> Some ((name, Plus (first, second)))
        | Regex "(.*): ([^ ]+) - ([^ ]+)" [name; first; second] -> Some ((name, Minus (first, second)))
        | Regex "(.*): ([^ ]+) \* ([^ ]+)" [name; first; second] -> Some ((name, Times (first, second)))
        | Regex "(.*): ([^ ]+) / ([^ ]+)" [name; first; second] -> Some ((name, Divide (first, second)))
        | s -> eprintfn "Unknown monkey: %s" s; None)
    |> Map.ofList

let getMonkey allMonkeys name = 
    match allMonkeys |> Map.tryFind name with
    | Some foo -> foo
    | None -> failwithf "can't find monkey %s" name

let rec evaluate allMonkeys = function
    | Raw value -> value
    | Plus (first, second) -> (evaluate allMonkeys (getMonkey allMonkeys first)) + (evaluate allMonkeys (getMonkey allMonkeys second))
    | Minus (first, second) -> (evaluate allMonkeys (getMonkey allMonkeys first)) - (evaluate allMonkeys (getMonkey allMonkeys second))
    | Times (first, second) -> (evaluate allMonkeys (getMonkey allMonkeys first)) * (evaluate allMonkeys (getMonkey allMonkeys second))
    | Divide (first, second) -> (evaluate allMonkeys (getMonkey allMonkeys first)) / (evaluate allMonkeys (getMonkey allMonkeys second))

let run input =
    Map.iter (fun k v -> printfn "%A: %A" k v) input
    evaluate input (getMonkey input "root")

doProcess parseInput run rawInput
