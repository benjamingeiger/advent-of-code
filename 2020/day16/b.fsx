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

let restrictionPattern = Regex @"^(.*): (\d+)-(\d+) or (\d+)-(\d+)"
let parseRestriction (s : string) =
    let m = restrictionPattern.Match s
    (m.Groups.[1].Value, int m.Groups.[2].Value, int m.Groups.[3].Value, int m.Groups.[4].Value, int m.Groups.[5].Value)

let restrictionValues = Seq.map parseRestriction restrictions

let parseTicket (s : string) =
    s.Split ","
    |> Array.map int
    |> Array.toList

let ticketValues = Seq.map parseTicket nearbyTickets

let validateField rs n =
    let validRestrictions =
        rs
        |> Seq.filter (fun (_, l1, h1, l2, h2) -> (l1 <= n && n <= h1) || (l2 <= n && n <= h2))
   
    not (Seq.isEmpty validRestrictions)

let validateTicket t = t |> Seq.map (validateField restrictionValues) |> Seq.reduce (&&)

let validTickets =
    ticketValues
    |> Seq.filter validateTicket

// from https://stackoverflow.com/questions/9213761/cartesian-product-two-lists
let cartesian xs ys = 
    xs |> Seq.collect (fun x -> ys |> Seq.map (fun y -> x, y))

let edges =
    let xs = restrictionValues
    let ys = [0 .. (Seq.length (Seq.head validTickets) - 1)]

    cartesian xs ys
    |> Seq.filter (fun ((d, l1, h1, l2, h2), n) ->
        Seq.map (Seq.item n) validTickets
        |> Seq.map (fun n' -> ((l1 <= n' && n' <= h1) || (l2 <= n' && n' <= h2)))
        |> Seq.reduce (&&))
    |> Seq.map (fun ((d, _, _, _, _), n) -> (d, (sprintf "%d" n)))
    |> List.ofSeq

let matchFields edges =
    let rec go (edges' : seq<string * string>) matches =
        let singleEdges =
            edges' 
            |> Seq.groupBy snd 
            |> Seq.filter (fun (_, y) -> Seq.length y = 1)
            |> Seq.map snd

        if (Seq.length singleEdges) = 1
        then
            let (d, c) = Seq.head (Seq.head singleEdges)
            printfn "%A" (d, c)
            let edges'' = edges' |> Seq.filter (fun (x, y) -> not (x = d || y = c))

            go edges'' ((d, c) :: matches)
        else
            if (Seq.length edges') = 0 then matches
            else failwith (sprintf "algo failed wtf %A %A" (edges' |> Seq.groupBy snd |> Seq.sortBy (fun (x, y) -> Seq.length y)) edges')

    go edges []

let fields = matchFields edges |> Seq.map (fun (d, c) -> d, int c)

printfn "%A" fields

let myTicketValue = myTicket |> parseTicket

let result =
    fields
    |> Seq.filter (fun (x, y) -> x.StartsWith "departure")
    |> Seq.map snd
    |> Seq.map (fun n -> myTicketValue.[n])
    |> Seq.map bigint
    |> Seq.reduce (*)

printfn "%A" result




