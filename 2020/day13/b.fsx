// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic 

// can't remember where I got this
let rec bigintsFrom (n : bigint) = seq { 
  yield n
  yield! bigintsFrom (n + 1I) }

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let tryParse (s : string) =
    match bigint.TryParse s with
    | (true, i) -> Some i
    | (false, _) -> None

let routeInput = Seq.head (Seq.tail input)
let routes =
    routeInput.Split "," 
    |> Array.map tryParse 
    |> Seq.zip (bigintsFrom 0I) 
    |> Seq.filter (fun (x, y) -> y.IsSome)
    |> Seq.map (fun (x, y) -> (x, y.Value))

// adapted from https://rosettacode.org/wiki/Modular_inverse#Haskell
// I couldn't figure out the F# implementation on that site
let rec eGCD a b =
    if b = 0I then (a, 1I, 0I)
    else
        let (g, s, t) = eGCD b (a % b)
        (g, t, s - (a / b) * t)

// this too
let rec modinv a m =
    let (g, s, t) = eGCD a m
    if g <> 1I then failwith (sprintf "invalid pair %A %A" a m)
    else
        if s < 0I then s + m else s

let crt (congruences : seq<bigint * bigint>) =
    let M =
        congruences 
        |> Seq.map snd 
        |> Seq.fold (fun a x -> a * x) 1I

    let As = congruences |> Seq.map fst

    let ys = congruences |> Seq.map (fun (a, m) -> modinv (M / m) m)

    let prods =
        congruences
        |> Seq.map (fun (a, m) -> M / m)
        |> Seq.zip3 As ys
        |> Seq.map (fun (a, b, c) -> a * b * c)

    let total = prods |> Seq.reduce (+)

    congruences, M, ys, prods, total % M, M - (total % M)

printfn "%A" (crt routes)

