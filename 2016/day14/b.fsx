// vim: set et ts=4 sw=4 list :

open System

open System.Text.RegularExpressions

let salt = "ahsbgdzn"

// borrowed from http://www.fssnip.net/3D/title/MD5-hash
open System.Security.Cryptography
open System.Text

let md5 (input : string) : string =
    let data = System.Text.Encoding.ASCII.GetBytes input

    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string

let stretchKey i =
    let rec go count value = if count = 0 then value else go (count - 1) (md5 value)
    go 2017 (sprintf "%s%d" salt i)

let hashStream = Seq.initInfinite (fun i -> (i, stretchKey i)) |> Seq.cache

let tripletPattern = @"(.)\1\1"
let tripletRegex = new Regex(tripletPattern)

let isKey (i, h) =
    let hasTriple =
        let m = tripletRegex.Match h
        if m.Success then Some m.Groups.[1].Value else None

    match hasTriple with
    | None -> false
    | Some d ->
        let quintuplet = String.replicate 5 d
        let potentials =
            hashStream
            |> Seq.skip (i + 1)
            |> Seq.take 1000

        match Seq.tryFind (fun (i', (h' : string)) -> h'.Contains(quintuplet)) potentials with
            | None -> false
            | Some _ -> true

let result =
    hashStream
    |> Seq.filter isKey
    |> Seq.item 63
    |> printfn "%A"
