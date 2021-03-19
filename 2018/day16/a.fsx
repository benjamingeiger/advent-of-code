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
let samplesText =
    readLines "input.txt"
    |> String.concat "/"
    |> fun s -> s.Split("////")
    |> fun a -> a.[0]

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

    [ addr; addi; mulr; muli; banr; bani; borr; bori;
      setr; seti; gtir; gtri; gtrr; eqir; eqri; eqrr]
    |> List.concat
    |> List.filter ((=) afterVal)
    |> List.length

let result =
    samples
    |> Seq.map (fun (before, command, after) -> check before command after)
    |> Seq.filter ((<=) 3)
    |> Seq.length

printfn "%A" result
