// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> List.ofSeq

let getLocation (input : string list) (r : int) (c : int) =
    let c' = c % input.[r].Length

    input.[r].[c']

let countTrees dr dc =
    [0 .. dr .. (input.Length - 1)]
    |> List.map (fun r -> (r, (r / dr) * dc))
    |> List.map (fun (r, c) -> getLocation input r c)
    |> List.filter (fun c -> c = '#')
    |> List.length

let treesOnSlopes =
    [(1, 1); (1, 3); (1, 5); (1, 7); (2, 1)]
    |> List.map (fun (dr, dc) -> countTrees dr dc)

treesOnSlopes
    |> List.map bigint
    |> List.reduce (*)
    |> printfn "%A"
    
