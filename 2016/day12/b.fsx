// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

type RegisterAddress = A | B | C | D
type Storage = Map<RegisterAddress, int>
type Operand = Register of RegisterAddress | Value of int
type Operator =
    | CPY of Operand * Operand
    | INC of Operand
    | DEC of Operand
    | JNZ of Operand * Operand
type Program = List<Operator>

let run program =
    let rec step storage ip =
        (*printfn "Processing: %A %A" (List.tryItem ip program) storage*)
        match List.tryItem ip program with
        | None -> storage
        | Some (CPY (Value x, Register y)) -> step (Map.add y x storage) (ip + 1)
        | Some (CPY (Register x, Register y)) -> step (Map.add y (Map.find x storage) storage) (ip + 1)
        | Some (INC (Register x)) -> step (Map.add x ((Map.find x storage) + 1) storage) (ip + 1)
        | Some (DEC (Register x)) -> step (Map.add x ((Map.find x storage) - 1) storage) (ip + 1)
        | Some (JNZ (x, y)) -> 
            let xValue =
                match x with
                | Value x' -> x'
                | Register x' -> Map.find x' storage

            let destination =
                match y with
                | Value y' -> ip + y'
                | Register y' -> ip + (Map.find y' storage)

            if xValue <> 0
            then step storage destination
            else step storage (ip + 1)
        | Some inst -> failwith (sprintf "invalid instruction %A" inst)

    let initStorage = [(A, 0); (B, 0); (C, 1); (D, 0)] |> Map.ofList

    step initStorage 0

let program =
    let parseOperand s =
        match s with
        | "a" | "A" -> Register A
        | "b" | "B" -> Register B
        | "c" | "C" -> Register C
        | "d" | "D" -> Register D
        | _ -> Value (int s)

    let parseLine (l : string) =
        let parts = l.Split([| " " |], StringSplitOptions.RemoveEmptyEntries)

        match parts.[0] with
        | "cpy" -> CPY (parseOperand parts.[1], parseOperand parts.[2])
        | "inc" -> INC (parseOperand parts.[1])
        | "dec" -> DEC (parseOperand parts.[1])
        | "jnz" -> JNZ (parseOperand parts.[1], parseOperand parts.[2])
        | _ -> failwith (sprintf "invalid line %A" l)

    input |> Seq.map parseLine |> Seq.toList

printfn "%A" (run program)
