// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

let hash key index =
    use md5 = System.Security.Cryptography.MD5.Create()
    
    let body =
        (sprintf "%s%d" key index)
        |> System.Text.Encoding.ASCII.GetBytes
        |> md5.ComputeHash
        |> Seq.map (fun c -> c.ToString("X2"))
        |> Seq.reduce (+)

    body

let key = (Seq.head input)

let results =
    Seq.initInfinite id
        |> Seq.map (hash key)
        |> Seq.filter (fun (h : string) -> h.StartsWith "00000")
        |> Seq.map (fun (h : string) -> h.[5])
        |> Seq.take 8
        |> Seq.map string
        |> String.concat ""

printfn "%A" results
