// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic 
open System.Text.RegularExpressions

let enumerateSeq (s : seq<'a>) = Seq.zip (Seq.initInfinite id) s

let split (delimiter : char) (s : string) =
    s.Split(delimiter, (StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries))
    |> Array.toList

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

let inputParts = input |> splitSeq ((=) "") |> Seq.toList
let ruleText = inputParts.[0]
let messageText = inputParts.[1]

type Rule = Terminal of string | Nonterminal of int | Sequence of Rule list | Alternate of Rule list

let parseRule (s : string) =
    let (ruleNumber, bodyText) = s.Split ": " |> (fun a -> (int a.[0]), a.[1])

    let rec parseSubrule (s' : string) =
        let s'' = s'.Trim()
        if s''.[0] = '"' then Terminal (s''.[1..(s''.Length - 2)])
        else if s''.Contains '|' then Alternate (s'' |> split '|' |> List.map parseSubrule)
        else if s''.Contains ' ' then Sequence (s'' |> split ' ' |> List.map parseSubrule)
        else Nonterminal (int s'')

    (ruleNumber, parseSubrule bodyText)

let rules = ruleText |> Seq.map parseRule |> Map.ofSeq


