// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt"

let findCorruption s =
    let rec step stack = function
        | [] -> None
        | h :: t ->
            match h with
            | '(' | '[' | '{' | '<' -> step (h :: stack) t
            | ')' -> if List.head stack = '(' then step (List.tail stack) t else Some ')'
            | ']' -> if List.head stack = '[' then step (List.tail stack) t else Some ']'
            | '}' -> if List.head stack = '{' then step (List.tail stack) t else Some '}'
            | '>' -> if List.head stack = '<' then step (List.tail stack) t else Some '>'
            | _  ->
                printfn "unknown char: %A" h
                step stack t

    step [] (List.ofSeq s)

let result =
    input
    |> Seq.choose findCorruption
    |> Seq.map (function
        | ')' -> 3
        | ']' -> 57
        | '}' -> 1197
        | '>' -> 25137
        | _ -> 0)
    |> Seq.sum

printfn "%A" result

