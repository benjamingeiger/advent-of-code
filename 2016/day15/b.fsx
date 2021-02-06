// vim: set et ts=4 sw=4 list :

open System

(*let input = [(1, 5, 4); (2, 2, 1)]*)
let input = [(1, 17, 15); (2, 3, 2); (3, 19, 4); (4, 13, 2); (5, 7, 2); (6, 5, 0); (7, 11, 0)]

// adapted from https://rosettacode.org/wiki/Modular_inverse#Haskell
// I couldn't figure out the F# implementation on that site
let rec eGCD a b =
    if b = 0 then (a, 1, 0)
    else
        let (g, s, t) = eGCD b (a % b)
        (g, t, s - (a / b) * t)

// this too
let rec modinv a m =
    let (g, s, t) = eGCD a m
    if g <> 1 then failwith (sprintf "invalid pair %A %A" a m)
    else
        if s < 0 then s + m else s

let crt (congruences : seq<int * int>) =
    let M =
        congruences 
        |> Seq.map snd 
        |> Seq.fold (fun a x -> a * x) 1

    let As = congruences |> Seq.map fst

    let ys = congruences |> Seq.map (fun (a, m) -> modinv (M / m) m)

    let prods =
        congruences
        |> Seq.map (fun (a, m) -> M / m)
        |> Seq.zip3 As ys
        |> Seq.map (fun (a, b, c) -> a * b * c)

    let total = prods |> Seq.reduce (+)

    congruences, M, ys, prods, total % M, M - (total % M)

let congruences =
    input |> List.map (fun (n, m, o) -> (((2 * m - o) - n) % m, m))

let result = crt congruences

printfn "%A" result
