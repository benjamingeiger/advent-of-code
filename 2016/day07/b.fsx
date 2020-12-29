// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let parse s =
    let rec outer abas babs p1 p2 s' =
        if (Seq.isEmpty s') then (Set.ofSeq abas, Set.ofSeq babs)
        else
            match (Seq.head s') with
            | '[' -> inner abas babs '[' '[' (Seq.tail s')
            | ']' -> outer abas babs ']' ']' (Seq.tail s')
            | c when c = p1 && c <> p2 -> outer ((p1, p2, p1) :: abas) babs p2 c (Seq.tail s')
            | c -> outer abas babs p2 c (Seq.tail s')
    and inner abas babs p1 p2 s' =
        if (Seq.isEmpty s') then (Set.ofSeq abas, Set.ofSeq babs)
        else
            match (Seq.head s') with
            | ']' -> outer abas babs ']' ']' (Seq.tail s')
            | '[' -> inner abas babs '[' '[' (Seq.tail s')
            | c when c = p1 && c <> p2 -> inner abas ((p2, p1, p2) :: babs) p2 c (Seq.tail s')
            | c -> inner abas babs p2 c (Seq.tail s')

    outer [] [] '_' '_' s

let results =
    input
    |> Seq.map parse
    |> Seq.map (fun (abas, babs) -> Set.intersect abas babs)
    |> Seq.filter (fun s -> Set.count s > 0)
    |> Seq.length
    
printfn "%A" results

        
