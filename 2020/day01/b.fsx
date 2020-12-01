let rec pairs l =
    match l with
    | [] | [_] -> []
    | x :: xs -> 
        [for x' in xs do
            yield x,x'
         yield! pairs xs]

let rec triples l =
    match l with
    | [] | [_] | [_;_] -> []
    | x :: xs ->
        [for (x', y') in (pairs xs) do
            yield x,x',y'
         yield! triples xs]

let readLines filePath = System.IO.File.ReadLines(filePath);;

let expenses = readLines "input.txt" |> Seq.map int |> List.ofSeq

let sums = triples expenses
    |> List.filter (fun (x, y, z) -> x + y + z = 2020)
    |> List.map (fun (x, y, z) -> x * y * z)

printfn "%A" sums

// vim: set et ts=4 sw=4 list :
