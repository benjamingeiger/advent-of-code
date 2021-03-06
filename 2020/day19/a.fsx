// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic 
open System.Text.RegularExpressions

let enumerateSeq (s : seq<'a>) = Seq.zip (Seq.initInfinite id) s

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
let input = readLines "input.part1.txt"

let inputParts = input |> splitSeq ((=) "") |> Seq.toList
let ruleText = inputParts.[0]
let messageText = inputParts.[1]

printfn "%A" ruleText
printfn "%A" messageText

type Rule = Literal of string | Sequence of int list | Alternate of Rule list

let parseRule (s : string) =
    let (ruleNumber, bodyText) = s.Split ": " |> (fun a -> (int a.[0]), a.[1])

    let rec parseSubrule (s' : string) =
        let s'' = s'.Trim()
        if s''.[0] = '"' then Literal (s''.[1..(s''.Length - 2)])
        else if s''.Contains '|' then
            Alternate (s''.Split '|' |> Array.map parseSubrule |> Array.toList)
        else
            Sequence (s''.Split ' ' |> Array.map int |> Array.toList)

    (ruleNumber, parseSubrule bodyText)

printfn "%A" (Seq.head ruleText |> parseRule)

let rules = ruleText |> Seq.map parseRule |> Map.ofSeq

printfn "%A" rules

let buildRegex rules =
    let wrap s = sprintf "(%s)" s

    let rec build = function
        | Literal t -> wrap t
        | Alternate rs -> rs |> List.map build |> String.concat "|" |> wrap
        | Sequence is -> is |> List.map lookup |> String.concat "" |> wrap
    and lookup index =
        match Map.tryFind index rules with
        | None -> failwith (sprintf "unable to find key %A" index)
        | Some r -> build r

    sprintf "^%s$" (lookup 0)

let finalRegex = buildRegex rules

let results = messageText |> Seq.map (fun m -> Regex.IsMatch(m, finalRegex)) |> Seq.filter id |> Seq.length

printfn "%A" results
