// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt"

let completeLine s =
    let rec step stack = function
        | [] ->
            stack
            |> List.map (function
                | '(' -> ")"
                | '[' -> "]"
                | '{' -> "}"
                | '<' -> ">"
                | x -> failwithf "lolwut %A" x)
            |> String.concat ""
            |> Some

        | h :: t ->
            match h with
            | '(' | '[' | '{' | '<' -> step (h :: stack) t
            | ')' -> if List.head stack = '(' then step (List.tail stack) t else None
            | ']' -> if List.head stack = '[' then step (List.tail stack) t else None
            | '}' -> if List.head stack = '{' then step (List.tail stack) t else None
            | '>' -> if List.head stack = '<' then step (List.tail stack) t else None
            | _  ->
                printfn "unknown char: %A" h
                step stack t

    step [] (List.ofSeq s)

let result =
    input
    |> Seq.choose completeLine
    |> Seq.map (Seq.map (function
        | ')' -> 1I
        | ']' -> 2I
        | '}' -> 3I
        | '>' -> 4I
        | _ -> 0I))
    |> Seq.map (Seq.fold (fun acc x -> acc * 5I + x) 0I)
    |> List.ofSeq
    |> List.sort
    |> fun l -> List.zip l (List.rev l)
    |> List.filter (fun (x, y) -> x = y)
    |> List.exactlyOne
    |> fst

printfn "%A" result

