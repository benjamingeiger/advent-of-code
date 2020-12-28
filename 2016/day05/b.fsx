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

let fillPassword clues =
    let completePassword seen =
        [0..7]
        |> Seq.map (fun i -> Map.find i seen)
        |> Seq.map string
        |> String.concat ""

    let rec go seen inputStream =
        printfn "%A %A" (Map.count seen) (Seq.head inputStream)

        if (Map.count seen) = 8 then completePassword seen
        else
            let (pos, c) = Seq.head inputStream
            let t = Seq.tail inputStream

            if Map.containsKey pos seen
            then go seen t
            else go (Map.add pos c seen) t

    go (Map.empty) clues

let results =
    Seq.initInfinite id
        |> Seq.map (hash (Seq.head input))
        |> Seq.filter (fun (h : string) -> h.StartsWith "00000" && (h.[5] >= '0' && h.[5] <= '7'))
        |> Seq.map (fun (h : string) -> ((int h.[5]) - (int '0'), h.[6]))
        |> fillPassword

printfn "%A" results
