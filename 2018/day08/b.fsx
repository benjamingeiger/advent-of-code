// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input =
    readLines "input.txt"
    |> Seq.head
    |> (fun (s : string) -> s.Split(" "))
    |> Seq.map int
    |> Seq.toList

type Tree = Tree of Tree list * int list

let rec parse = function
    | [] | [_] -> failwith "invalid header"
    | numChildren :: numMetadata :: rest ->
        let (children, rest') = List.mapFold (fun t _ -> parse t) rest [1..numChildren]
        let (metadata, rest'') = List.splitAt numMetadata rest'

        Tree (children, metadata), rest''

let tree = parse input |> fst

let rec sumMetadata = function
    | Tree ([], metadata) -> List.sum metadata
    | Tree (children, metadata) ->
        metadata
        |> List.map (fun i ->
            match List.tryItem (i - 1) children with
            | Some tree -> sumMetadata tree
            | None -> 0)
        |> List.sum

printfn "%A" <| sumMetadata tree

