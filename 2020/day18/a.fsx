// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic 

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

type Token = Number of bigint | Add | Mul | LParen | RParen

let digits = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'] |> Set.ofList

let lex (s : string) =
    let joinPartial p =
        p
        |> List.rev
        |> List.fold (+) ""
        |> bigint.Parse

    let rec step (partial, tokens) c =
        if not (Set.contains c digits) && partial <> [] then
            step ([], Number (joinPartial partial) :: tokens) c
        else
            match c with
            | c' when (Set.contains c' digits) -> (partial @ [string c], tokens)
            | '+' -> ([], Add :: tokens)
            | '*' -> ([], Mul :: tokens)
            | '(' -> ([], LParen :: tokens)
            | ')' -> ([], RParen :: tokens)
            | ' ' -> ([], tokens)
            | _ -> failwith (sprintf "invalid char %A" c)

    Seq.fold step ([], []) s
        |> fun (partial, tokens) -> if partial = [] then tokens else Number (joinPartial partial) :: tokens 
        |> Seq.rev
        |> Seq.toList

let shunt tokens =
    let rec go output operators = function
        | [] -> (List.rev output) @ operators
        | Number i :: t -> go (Number i :: output) operators t
        | LParen :: t -> go output (LParen :: operators) t
        | RParen :: t ->
            if operators = [] then failwith "empty stack"
            else 
                if (List.head operators) = LParen
                then go output (List.tail operators) t
                else go ((List.head operators) :: output) (List.tail operators) (RParen :: t)
        | Add :: t ->
            if operators <> [] && (List.head operators) <> LParen
            then go ((List.head operators) :: output) (List.tail operators) (Add :: t)
            else go output (Add :: operators) t
        | Mul :: t ->
            if operators <> [] && (List.head operators) <> LParen
            then go ((List.head operators) :: output) (List.tail operators) (Mul :: t)
            else go output (Mul :: operators) t

    go [] [] tokens

let evaluate tokens =
    let step stack token = 
        match (stack, token) with
        | _, Number i -> i :: stack
        | a :: b :: t, Add -> a + b :: t
        | a :: b :: t, Mul -> a * b :: t
        | _ -> failwith (sprintf "Invalid state %A" (stack, token))

    List.fold step [] tokens

let compute = evaluate << shunt << lex
let results = Seq.map compute input

printfn "%A" (Seq.reduce (+) (Seq.map Seq.head results))
