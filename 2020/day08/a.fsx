// vim: set et ts=4 sw=4 list :

open System

let contains x = Seq.exists ((=) x)

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

type Instruction =
    | Acc of int
    | Jmp of int
    | Nop

let parseInstruction (s : string) =
    let parts = s.Split " " |> Array.toList

    let argument = int (parts.[1])

    match parts.[0] with
    | "acc" -> Acc argument
    | "jmp" -> Jmp argument
    | "nop" -> Nop
    | _ -> failwith "Unexpected operation"

let instructions =
    input
    |> Seq.map parseInstruction
    |> List.ofSeq

let execute (insts : Instruction list) =
    let rec step acc ip history =
        if contains ip history
        then acc
        else
            match insts.[ip] with
            | Acc arg -> step (acc + arg) (ip + 1) (ip :: history)
            | Jmp arg -> step acc (ip + arg) (ip :: history)
            | Nop -> step acc (ip + 1) (ip :: history)

    step 0 0 []

let result = execute instructions

printfn "%A" result
