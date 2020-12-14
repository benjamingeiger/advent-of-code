// vim: set et ts=4 sw=4 list :

open Microsoft.FSharp.Core.Operators.Checked
open System
open System.Collections.Generic 

type line = Mask of string | Assignment of uint64 * uint64

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

let commands = input |> Seq.map parseCommand

let rec genMaskedValues bitMask value =
    let posMask =
        bitMask
        |> Seq.map (fun c -> if c = '1' then 1UL else 0UL)
        |> Seq.fold (fun acc b -> acc * 2UL + b) 0UL

    let value' = value ||| posMask

    let rec floatBits bitMask value (results : uint64 list) =
        if bitMask = "" then results
        else
            floatBits (bitMask.[..(bitMask.Length - 2)]) (value / 2UL)
                (results |> match bitMask.[bitMask.Length - 1] with
                            | '0' -> List.map (fun a -> a * 2UL + (value % 2UL))
                            | '1' -> List.map (fun a -> a * 2UL + 1UL)
                            | 'X' -> List.collect (fun a -> [a * 2UL; a * 2UL + 1UL])
                            | c -> failwith (sprintf "invalid bitmask char %A" c))

    floatBits bitMask value' [0UL]

let updateRam curMask address value ram =
    let rec updateRamCells ram addresses =
        match addresses with
        | [] -> ram
        | h :: t -> updateRamCells (Map.add h value ram) t

    updateRamCells ram (genMaskedValues curMask address)

let step (curMask, ram : Map<uint64, uint64>) command =
    match command with
    | Mask s -> (s, ram)
    | Assignment (address, value) -> (curMask, (updateRam curMask address value ram))

let result =
    commands
    |> Seq.fold step ("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", Map.empty)
    |> snd
    |> Map.fold (fun acc addr value -> acc + value) 0UL

printfn "%A" result
