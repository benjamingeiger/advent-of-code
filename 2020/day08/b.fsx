// vim: set et ts=4 sw=4 list :

open System

let contains x = Seq.exists ((=) x)

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

type Instruction =
    | Acc of int
    | Jmp of int
    | Nop of int

let parseInstruction (s : string) =
    let parts = s.Split " " |> Array.toList

    let argument = int (parts.[1])

    match parts.[0] with
    | "acc" -> Acc argument
    | "jmp" -> Jmp argument
    | "nop" -> Nop argument
    | _ -> failwith (sprintf "Unexpected operation: %s" parts.[0])

let instructions =
    input
    |> Seq.map parseInstruction
    |> List.ofSeq

// Run the provided instructions until either an instruction is
// executed twice or we run off the end.
let execute (insts : Instruction list) =
    let rec step acc ip history =
        if contains ip history then None
        else if (ip >= List.length insts) then Some acc
        else
            match insts.[ip] with
            | Acc arg -> step (acc + arg) (ip + 1) (ip :: history)
            | Jmp arg -> step acc (ip + arg) (ip :: history)
            | Nop arg -> step acc (ip + 1) (ip :: history)

    step 0 0 []

// Flip the nth instruction from JMP to NOP or vice versa. Do
// nothing if the nth instruction is ACC.
let rec toggle n insts =
    let flip inst =
        match inst with
        | Acc arg -> Acc arg
        | Jmp arg -> Nop arg
        | Nop arg -> Jmp arg

    match insts with
    | [] -> []
    | h :: t ->
        if n = 0 then (flip h) :: t else (h :: toggle (n - 1) t)

// Yeah, this is severely unoptimized. Yes, it'll even rerun if
// the flip doesn't actually flip anything (that might be a good
// first optimization).
//
// Basically, it'll flip each JMP/NOP in turn and filter out the
// runs where execute returns None, indicating a cycle. Given a
// proper input, it should only have one element in the resulting
// list. That'd be a second optimization. (Something something
// Haskell something lazy something something.)
let exhaustiveSearch =
    [0..(List.length instructions)]
    |> List.map (fun n -> execute (toggle n instructions))
    |> List.filter (fun n -> n.IsSome)

printfn "%A" exhaustiveSearch
