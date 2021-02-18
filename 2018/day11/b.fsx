// vim: set et ts=4 sw=4 list :

open System

let input = 1308

let initGrid input x y =
    let rackID = x + 10

    (((y * rackID + input) * rackID) % 1000) / 100 - 5

let grid = Array2D.initBased 1 1 300 300 (initGrid input)

let sumGrid = Array2D.initBased 1 1 300 300 (fun _ _ -> (None : int option))

let rec getSum (grid : int[,]) (sumGrid : int option[,]) x y =
    let recursive = getSum grid sumGrid

    if x < 1 || y < 1 then 0
    else
        match sumGrid.[x, y] with
        | None ->
            let sum = grid.[x, y] + (recursive x (y - 1)) + (recursive (x - 1) y) - (recursive (x - 1) (y - 1))
            sumGrid.[x, y] <- Some sum
            sum
        | Some sum -> sum

let powerLevel getSum size x y =
    (getSum x y)
    + (getSum (x + size) (y + size))
    - (getSum (x + size) y)
    - (getSum x (y + size))

let result =
    let getSum' = getSum grid sumGrid

    [1..300]
    (*[3]*)
    |> List.collect (fun size ->
        List.allPairs [1..(300 - size)] [1..(300 - size)] 
        |> List.map (fun (x, y) -> (size, x, y)))
    |> List.map (fun (size, x, y) -> (powerLevel getSum' size x y, x, y, size))
    |> List.sortBy (fun (r, _, _, _) -> -r)
    |> List.head
    |> fun (r, x, y, size) -> (r, x + 1, y + 1, size)

printfn "%A" result
