let readLines filePath = System.IO.File.ReadLines(filePath);;
let expenses = readLines "input.txt" |> Seq.map int |> List.ofSeq

let rec pairs l =
    match l with
    | [] | [_] -> []
    | x :: xs -> 
        [for x' in xs do
            yield x,x'
         yield! pairs xs]

let sums = pairs expenses
    |> List.filter (fun (x, y) -> x + y = 2020)
    |> List.map (fun (x, y) -> x * y)

printfn "%A" sums
