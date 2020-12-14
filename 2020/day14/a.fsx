// vim: set et ts=4 sw=4 list :

open Microsoft.FSharp.Core.Operators.Checked
open System
open System.Collections.Generic 

type Command = Mask of string | Assignment of uint64 * uint64

let readLines filePath = System.IO.File.ReadAllLines filePath
let input = readLines "input.txt"

let parseCommand (s : string) =
    match s.[..2] with
    | "mas" -> Mask (s.[7..])
    | "mem" ->
        let parts = s.Split " = "
        let value = uint64 parts.[1]
        let address = uint64 (parts.[0].[4..(parts.[0].Length - 2)])

        Assignment (address, value)
    | x -> failwith (sprintf "invalid line %A" s)

let commands =
    input
    |> Seq.map parseCommand

let mask (bitmask : string) value =
    let posMask =
        bitmask 
        |> Seq.map (fun c -> if c = '1' then 1UL else 0UL)
        |> Seq.fold (fun acc b -> acc * 2UL + b) 0UL

    let negMask =
        bitmask
        |> Seq.map (fun c -> if c = '0' then 0UL else 1UL)
        |> Seq.fold (fun acc b -> acc * 2UL + b) 0UL

    (value &&& negMask) ||| posMask


let step (curMask, ram : Map<uint64, uint64>) command =
    let output =
        match command with
        | Mask s -> (s, ram)
        | Assignment (address, value) -> (curMask, Map.add address (mask curMask value) ram)

    output

let result =
    commands
    |> Seq.fold step ("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", Map.empty)
    |> snd
    |> Map.fold (fun acc addr value -> acc + value) 0UL

printfn "%A" result
