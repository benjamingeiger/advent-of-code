// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.map Seq.toList |> Seq.toList

let cells = Array2D.init (List.length input) (List.length (List.head input)) (fun r c -> input.[r].[c])

printfn "%A" cells

type Direction = Down | Left | Up | Right

let scan (cells : char[,]) =
    let start = (0, Array.findIndex ((=) '|') cells.[0, *])

    let step (r, c) = function
        | Down -> (r + 1, c)
        | Left -> (r, c - 1)
        | Up -> (r - 1, c)
        | Right -> (r, c + 1)

    let turns (r, c) = function
        | Down -> [(Left, step (r, c) Left); (Right, step (r, c) Right)]
        | Left -> [(Up, step (r, c) Up); (Down, step (r, c) Down)]
        | Up -> [(Left, step (r, c) Left); (Right, step (r, c) Right)]
        | Right -> [(Up, step (r, c) Up); (Down, step (r, c) Down)]

    let isOnRoad (r, c) =
        if r < 0 || c < 0 || r >= Array2D.length1 cells || r >= Array2D.length2 cells
        then false
        else cells.[r, c] <> ' '

    let rec go seen direction (r, c) =
        if (not (isOnRoad (r, c)))
        then seen
        else
            let cur = cells.[r, c]
            if cur = '+'
            then
                turns (r, c) direction
                |> List.filter (snd >> isOnRoad)
                |> function
                    | [] -> seen
                    | [(nextTurn, (r', c'))] ->
                        go (cur :: seen) nextTurn (r', c')
                    | _ -> failwith "invalid input"
            else go (cur :: seen) direction (step (r, c) direction)

    go [] Down start

let result = scan cells |> List.length

printfn "%A" result
