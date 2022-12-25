// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

let parseInput rawInput =
    rawInput
    |> Seq.map (Seq.choose (function
        | '2' -> Some 2I
        | '1' -> Some 1I
        | '0' -> Some 0I
        | '-' -> Some -1I
        | '=' -> Some -2I
        | x -> printfn "invalid digit %c" x; None))
    |> Seq.map (Seq.fold (fun total cur -> total * 5I + cur) 0I)
    |> Seq.sum

let rec numberToSnafu d =
    if d = -2I then ['=']
    elif d = -1I then ['-']
    elif d = 0I then ['0']
    elif d = 1I then ['1']
    elif d = 2I then ['2']
    elif d % 5I = 0I then '0' :: numberToSnafu (d / 5I)
    elif d % 5I = 1I then '1' :: numberToSnafu ((d - 1I) / 5I)
    elif d % 5I = 2I then '2' :: numberToSnafu ((d - 2I) / 5I)
    elif d % 5I = 3I then '=' :: numberToSnafu ((d + 2I) / 5I)
    elif d % 5I = 4I then '-' :: numberToSnafu ((d + 1I) / 5I)
    else failwithf "how did we end up with %A?" d

let run input =
    input
    |> numberToSnafu
    |> List.rev
    |> List.map string
    |> String.concat ""

doProcess parseInput run rawInput
