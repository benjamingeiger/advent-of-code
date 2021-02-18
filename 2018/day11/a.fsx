// vim: set et ts=4 sw=4 list :

open System

let input = 1308
(*let input = 42*)

let initGrid input x y =
    let rackID = (x + 1) + 10

    ((((y + 1) * rackID + input) * rackID) % 1000) / 100 - 5

let grid = Array2D.init 300 300 (initGrid input)

let powerLevel (x, y) =
    let cells = [(x - 1, y - 1); (x - 1, y); (x - 1, y + 1); 
                 (x,     y - 1);     (x, y);     (x, y + 1);
                 (x + 1, y - 1); (x + 1, y); (x + 1, y + 1)]

    cells
    |> List.map (fun (x', y') -> grid.[x', y'])
    |> List.sum

let result =
    List.allPairs [1..298] [1..298]
    |> List.map (fun (x, y) -> ((x, y), powerLevel (x, y)))
    |> List.maxBy snd

printfn "%A" result
