// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let ipText = Seq.head input
let ipRegister = Seq.last ipText |> (fun c -> int c - int '0')

let programText = Seq.tail input

type Op =
    | ADDR // 14
    | ADDI //  4
    | MULR // 15
    | MULI //  8
    | BANR // 13
    | BANI // 10
    | BORR // 11
    | BORI //  9
    | SETR //  5
    | SETI //  1
    | GTIR // 12
    | GTRI //  7
    | GTRR //  6
    | EQIR //  0
    | EQRI //  2
    | EQRR //  3

let program =
    programText
    |> Seq.choose (function
        | Regex @"(\w\w\w\w) (\d+) (\d+) (\d+)" [opcode'; input1; input2; output] ->
            let opcode = 
                match opcode' with
                | "addr" -> ADDR | "addi" -> ADDI | "mulr" -> MULR | "muli" -> MULI
                | "banr" -> BANR | "bani" -> BANI | "borr" -> BORR | "bori" -> BORI
                | "setr" -> SETR | "seti" -> SETI | "gtir" -> GTIR | "gtri" -> GTRI
                | "gtrr" -> GTRR | "eqir" -> EQIR | "eqri" -> EQRI | "eqrr" -> EQRR
                | _ -> failwithf "invalid opcode %s" opcode'

            Some (opcode, int input1, int input2, int output)
        | s ->
            printfn "invalid line [%s]" s
            None)

let run ipRegister program =
    let tick storage (opcode, input1, input2, output) =
        match opcode with
        | ADDR ->
            let i1 = Map.find input1 storage
            let i2 = Map.find input2 storage

            Map.add output (i1 + i2) storage

        | ADDI ->
            let i1 = Map.find input1 storage

            Map.add output (i1 + input2) storage

        | MULR ->
            let i1 = Map.find input1 storage
            let i2 = Map.find input2 storage

            Map.add output (i1 * i2) storage

        | MULI ->
            let i1 = Map.find input1 storage

            Map.add output (i1 * input2) storage

        | BANR ->
            let i1 = Map.find input1 storage
            let i2 = Map.find input2 storage

            Map.add output (i1 &&& i2) storage

        | BANI ->
            let i1 = Map.find input1 storage

            Map.add output (i1 &&& input2) storage

        | BORR ->
            let i1 = Map.find input1 storage
            let i2 = Map.find input2 storage

            Map.add output (i1 ||| i2) storage

        | BORI ->
            let i1 = Map.find input1 storage

            Map.add output (i1 ||| input2) storage

        | SETR ->
            let i1 = Map.find input1 storage

            Map.add output i1 storage

        | SETI ->
            Map.add output input1 storage

        | GTIR ->
            let i2 = Map.find input2 storage

            Map.add output (if input1 > i2 then 1 else 0) storage

        | GTRI ->
            let i1 = Map.find input1 storage

            Map.add output (if i1 > input2 then 1 else 0) storage

        | GTRR ->
            let i1 = Map.find input1 storage
            let i2 = Map.find input2 storage

            Map.add output (if i1 > i2 then 1 else 0) storage

        | EQIR ->
            let i2 = Map.find input2 storage

            Map.add output (if input1 = i2 then 1 else 0) storage

        | EQRI ->
            let i1 = Map.find input1 storage

            Map.add output (if i1 = input2 then 1 else 0) storage

        | EQRR ->
            let i1 = Map.find input1 storage
            let i2 = Map.find input2 storage

            Map.add output (if i1 = i2 then 1 else 0) storage

    let initialStorage = [(0, 0); (1, 0); (2, 0); (3, 0); (4, 0); (5, 0)] |> Map.ofList

    let rec start storage program =
        let ip = Map.find ipRegister storage

        if ip < 0 || ip >= Array.length program
        then storage
        else
            let cur = program.[ip]
            let newStorage = tick storage cur
            let newStorage' = Map.add ipRegister ((Map.find ipRegister newStorage) + 1) newStorage 

            start newStorage' program

    start initialStorage (program |> Seq.toArray)

printfn "%A" program

let results = run ipRegister program

printfn "%A" results
