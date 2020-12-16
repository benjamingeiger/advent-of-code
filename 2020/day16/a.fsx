// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic
open System.Text.RegularExpressions

// borrowed from https://stackoverflow.com/questions/6736464/split-seq-in-f
let splitSeq p s =
    let i = ref 0
    s
    |> Seq.map (fun x -> 
        if p x then incr i
        !i, x)
    |> Seq.filter (fun (i, x) -> (not << p) x)
    |> Seq.groupBy fst
    |> Seq.map (fun (_, b) -> Seq.map snd b)

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let inputGroups = input |> splitSeq ((=) "")

let restrictions = inputGroups |> Seq.head
let myTicket = inputGroups |> Seq.tail |> Seq.head |> Seq.tail |> Seq.head
let nearbyTickets = inputGroups |> Seq.tail |> Seq.tail |> Seq.head |> Seq.tail

printfn "%A" restrictions
printfn "%A" myTicket
printfn "%A" nearbyTickets

let restrictionPattern = Regex @"^.*: (\d+)-(\d+) or (\d+)-(\d+)"
let parseRestriction (s : string) =
    let m = restrictionPattern.Match s
    [(int m.Groups.[1].Value, int m.Groups.[2].Value); (int m.Groups.[3].Value, int m.Groups.[4].Value)]

let restrictionValues = Seq.collect parseRestriction restrictions

printfn "%A" restrictionValues

let parseTicket (s : string) =
    s.Split ","
    |> Array.map int
    |> Array.toList

let ticketValues = Seq.map parseTicket nearbyTickets

printfn "%A" ticketValues

let validateField n =
    let validRestrictions =
        restrictionValues
        |> Seq.filter (fun (l, h) -> l <= n && n <= h)
    
    if Seq.isEmpty validRestrictions then n else 0

let countErrors errors ticket =
    let newErrors =
        ticket
        |> Seq.map validateField
        |> Seq.fold (+) 0
    newErrors + errors

let result = Seq.fold countErrors 0 ticketValues
printfn "%A" result
