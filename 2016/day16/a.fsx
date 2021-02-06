// vim: set et ts=4 sw=4 list :

open System

let reverseString (s : string) = s |> Array.ofSeq |> Array.rev |> System.String
let pairs s =
    s
    |> Seq.chunkBySize 2
    |> Seq.choose (function [| x; y |] -> Some (x, y) | _ -> None)

let targetSize = 272
let initial = "10111011111001111"

let fill l init =
    let invert s = s |> reverseString |> String.map (fun c -> match c with '1' -> '0' | '0' -> '1' | _ -> '?')
    let dragon s = String.concat "0" [s; invert s]

    let rec go s =
        if String.length s > l
        then s.[0..(l - 1)]
        else go <| dragon s

    go init

let rec checksum s =
    if String.length s % 2 = 1 then s
    else
        s
        |> pairs
        |> Seq.map (function ('0', '0') | ('1', '1') -> "1" | _ -> "0")
        |> String.concat ""
        |> checksum

let result = checksum <| fill targetSize initial

printfn "%A" result
