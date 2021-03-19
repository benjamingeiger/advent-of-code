// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let registerRegex = @"(Before|After):\s*\[(\d+), (\d+), (\d+), (\d+)\]"
let opcodeRegex = @"(\d+) (\d+) (\d+) (\d+)"
let fullRegex = String.concat "/" [registerRegex; opcodeRegex; registerRegex]

let readLines filePath = System.IO.File.ReadLines filePath
let (samplesText, programText) =
    readLines "input.txt"
    |> String.concat "/"
    |> fun s -> s.Split("////")
    |> fun a -> a.[0], a.[1]

let samples =
    samplesText
    |> fun s -> s.Split("//")
    |> Seq.choose (function
        | Regex fullRegex
            [_; before1; before2; before3; before4;
             operator; operand1; operand2; operand3;
             _; after1; after2; after3; after4] ->
                 let before =
                     [ int before1; int before2; int before3; int before4 ]
                     |> List.indexed
                     |> Map.ofList
                 let after =
                     [ int after1; int after2; int after3; int after4 ]
                     |> List.indexed
                     |> Map.ofList
                 let command = (int operator, int operand1, int operand2, int operand3)

                 Some (before, command, after)
        | s ->
            eprintfn "Invalid sample [%s]" s
            None)

let check before (_, input1, input2, output) after =

    let isRegister v = v >= 0 && v <= 3
    let isNotRegister = isRegister >> not
    let getRegister v = Map.find v before

    let beforeVal = Map.find output before
    let afterVal = Map.find output after

    let addr =
        if (isNotRegister input1 || isNotRegister input2)
        then []
        else [(getRegister input1) + (getRegister input2)]

    let addi =
        if (isNotRegister input1)
        then []
        else [(getRegister input1) + input2]

    let mulr =
        if (isNotRegister input1 || isNotRegister input2)
        then []
        else [(getRegister input1) * (getRegister input2)]

    let muli =
        if (isNotRegister input1)
        then []
        else [(getRegister input1) * input2]

    let banr =
        if (isNotRegister input1 || isNotRegister input2)
        then []
        else [(getRegister input1) &&& (getRegister input2)]

    let bani =
        if (isNotRegister input1)
        then []
        else [(getRegister input1) &&& input2]

    let borr =
        if (isNotRegister input1 || isNotRegister input2)
        then []
        else [(getRegister input1) ||| (getRegister input2)]

    let bori =
        if (isNotRegister input1)
        then []
        else [(getRegister input1) ||| input2]

    let setr =
        if (isNotRegister input1)
        then []
        else [(getRegister input1)]

    let seti = [input1]

    let gtrr =
        if (isNotRegister input1 || isNotRegister input2)
        then []
        else [if (getRegister input1) > (getRegister input2) then 1 else 0]

    let gtir =
        if isNotRegister input2
        then []
        else [if input1 > (getRegister input2) then 1 else 0]

    let gtri =
        if isNotRegister input1
        then []
        else [if (getRegister input1) > input2 then 1 else 0]

    let eqrr =
        if (isNotRegister input1 || isNotRegister input2)
        then []
        else [if (getRegister input1) = (getRegister input2) then 1 else 0]

    let eqir =
        if isNotRegister input2
        then []
        else [if input1 = (getRegister input2) then 1 else 0]

    let eqri =
        if isNotRegister input1
        then []
        else [if (getRegister input1) = input2 then 1 else 0]

    [ ("addr", addr); ("addi", addi); ("mulr", mulr); ("muli", muli);
      ("banr", banr); ("bani", bani); ("borr", borr); ("bori", bori);
      ("setr", setr); ("seti", seti); ("gtir", gtir); ("gtri", gtri); 
      ("gtrr", gtrr); ("eqir", eqir); ("eqri", eqri); ("eqrr", eqrr) ]
    |> List.choose (fun (name, value) -> if value = [afterVal] then Some name else None)
    |> Set.ofList

let findPossibilities samples =
    samples
    |> Seq.map (fun (before, command, after) -> check before command after)
    |> Set.intersectMany

let result =
    samples
    |> Seq.groupBy (fun (_, (opcode, _, _, _), _) -> opcode)
    |> Seq.map (fun (opcode, samples') -> (opcode, findPossibilities samples'))

let dump (opcode, possibilities) =
    printf "%d: " opcode
    Set.iter (printf "%s ") possibilities
    printfn ""

Seq.iter dump result

(*

4: addi
3: eqrr gtri seti
8: muli
2: eqri eqrr seti
11: borr
12: eqir eqri eqrr gtir gtri gtrr seti
15: addi mulr
6: eqir gtrr
5: addi addr bori borr gtir gtri gtrr seti setr
7: gtri seti
10: addi addr bani banr bori borr gtir gtri gtrr muli mulr seti setr
14: addi addr muli mulr
1: addi bori borr seti
9: bori borr
13: addi banr bori borr eqir eqrr gtri mulr seti setr
0: addr borr eqir eqri seti

*)

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
    programText.Split("/")
    |> Seq.choose (function
        | Regex @"(\d+) (\d+) (\d+) (\d+)" [opcode'; input1; input2; output] ->
            let opcode = 
                match opcode' with
                | "14" -> ADDR |  "4" -> ADDI | "15" -> MULR |  "8" -> MULI
                | "13" -> BANR | "10" -> BANI | "11" -> BORR |  "9" -> BORI
                |  "5" -> SETR |  "1" -> SETI | "12" -> GTIR |  "7" -> GTRI
                |  "6" -> GTRR |  "0" -> EQIR |  "2" -> EQRI |  "3" -> EQRR
                | _ -> failwithf "invalid opcode %s" opcode'

            Some (opcode, int input1, int input2, int output)
        | s ->
            printfn "invalid line [%s]" s
            None)

let run program =
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

    Seq.fold tick ([(0, 0); (1, 0); (2, 0); (3, 0)] |> Map.ofList) program

let results2 = run program

printfn "%A" (Map.find 0 results2)
