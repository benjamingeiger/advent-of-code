// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> Seq.cache

let drawSequence =
    input
    |> Seq.head
    |> fun (s : string) -> s.Split(",")
    |> Seq.map int
    |> Seq.toList

let parseBoard text =
    let digits =
        text
        |> Seq.map (fun (s : string) -> s.Trim().Split(" ", StringSplitOptions.RemoveEmptyEntries))
        |> Seq.map (Seq.map int)

    digits

let doesBoardWin moves (board : seq<seq<int>>) =
    let all = Seq.fold (&&) true
    let any = Seq.fold (||) false

    let rowWin spots row = spots |> Seq.item row |> all
    let rowsWin spots = [0..4] |> Seq.map (rowWin spots) |> any
    let colWin spots col = spots |> Seq.map (Seq.item col) |> all
    let colsWin spots = [0..4] |> Seq.map (colWin spots) |> any
    let mainDiagWin spots = spots |> Seq.indexed |> Seq.map (fun (i, r) -> Seq.item i r) |> all
    let offDiagWin spots = spots |> Seq.indexed |> Seq.map (fun (i, r) -> Seq.item (4 - i) r) |> all

    let boardWin spots =
        mainDiagWin spots || offDiagWin spots || rowsWin spots || colsWin spots

    let doesWin =
        board
        |> Seq.map (Seq.map (fun n -> List.contains n moves))
        |> boardWin

    if doesWin then Some board else None

let boards =
    input
    |> Seq.skip 2
    |> splitSeq ((=) "")
    |> Seq.map parseBoard
    |> Seq.cache

let firstWinner =
    let execute (movesSoFar, winningBoards) move =
        (move :: movesSoFar, Seq.choose (doesBoardWin (move :: movesSoFar)) boards)

    Seq.scan execute ([], Seq.empty) drawSequence
    |> Seq.find(fun (_, bs) -> Seq.length bs > 0)
    |> fun (ms, bs) -> (ms, Seq.head bs)

let calculateScore (moves, board) =
    board
    |> Seq.map (Seq.map (fun n -> if List.contains n moves then 0 else n))
    |> Seq.map (Seq.sum) |> Seq.sum
    |> ( * ) (List.head moves)

printfn "%A" (calculateScore firstWinner)
