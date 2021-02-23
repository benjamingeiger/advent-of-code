// vim: set et ts=4 sw=4 list :

open System

let inputA = 679I
let inputB = 771I

let step factor n =
    let next = (n * factor) % 2147483647I
    Some (next % 65536I, next)

let generate factor init = Seq.unfold (step factor) init

let generatorA = generate 16807I inputA |> Seq.filter (fun x -> x % 4I = 0I)
let generatorB = generate 48271I inputB |> Seq.filter (fun x -> x % 8I = 0I)

let result =
    Seq.zip generatorA generatorB
    |> Seq.take 5000000
    |> Seq.filter (fun (a, b) -> a = b)
    |> Seq.length

printfn "%A" result
