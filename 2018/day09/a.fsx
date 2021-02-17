// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

module Deque =
    type Deque<'a> = Empty | Deque of 'a list  * 'a list

    let empty = Empty

    let private flip = function
        | Empty -> Empty
        | Deque ([], front) -> Deque (List.rev front, [])
        | Deque (back, []) -> Deque ([], List.rev back)
        | Deque (front, back) -> Deque (front, back)

    let enqueueFront e = function
        | Empty -> Deque ([], [e])
        | Deque (back, front) -> Deque (back, e :: front)

    let enqueueBack e = function
        | Empty -> Deque ([], [e])
        | Deque (back, front) -> Deque (e :: back, front)

    let rec tryDequeueFront = function
        | Empty -> (None, Empty)
        | Deque (back, []) as deque -> tryDequeueFront (flip deque)
        | Deque (back, x :: xs) as deque -> (Some x, Deque (back, xs))

    let dequeueFront deque =
        match tryDequeueFront deque with
        | (None, _) -> failwith "Attempted to dequeue from empty queue"
        | (Some x, deque') -> (x, deque')

    let rec tryDequeueBack = function
        | Empty -> (None, Empty)
        | Deque ([], front) as deque -> tryDequeueBack (flip deque)
        | Deque (x :: xs, front) as deque -> (Some x, Deque (xs, front))

    let dequeueBack deque =
        match tryDequeueBack deque with
        | (None, _) -> failwith "Attempted to dequeue from empty queue"
        | (Some x, deque') -> (x, deque')

    let rec rotateForward n deque =
        match deque with
        | Empty -> Empty
        | Deque (back, []) -> rotateForward n (flip deque)
        | Deque (back, x :: xs) ->
            if n = 0 then Deque (back, x :: xs)
            elif n = 1 then Deque (x :: back, xs)
            else rotateForward (n - 1) (Deque (x :: back, xs))

    let rec rotateBack n deque =
        match deque with
        | Empty -> Empty
        | Deque ([], front) -> rotateBack n (flip deque)
        | Deque (x :: xs, front) ->
            if n = 0 then Deque (x :: xs, front)
            elif n = 1 then Deque (xs, x :: front)
            else rotateBack (n - 1) (Deque (xs, x :: front))

let readLines filePath = System.IO.File.ReadLines filePath
let (numPlayers, numMarbles) = readLines "input.txt" |> Seq.head |> function
    | Regex "(\d+) players; last marble is worth (\d+) points" [numPlayers'; numMarbles'] -> (int numPlayers', int numMarbles')
    | s -> failwith (sprintf "invalid input [%A]" s)

let findKeyWithDefault def key map =
    match Map.tryFind key map with
    | None -> def
    | Some value -> value

let insertMarble numPlayers (scores, circle) i =
    if i % 23 = 0 && i > 0
    then
        let circle' = Deque.rotateBack 7 circle
        let (popped, circle'') = Deque.dequeueFront circle'
        let currentPlayer = ((i + 1) % numPlayers)

        let addedPoints = i + popped
        let totalScore = (findKeyWithDefault 0 currentPlayer scores) + addedPoints

        printfn "%A points scored by player %A, total %A" addedPoints currentPlayer totalScore

        Map.add currentPlayer totalScore scores, circle''
    else
        let circle' = Deque.rotateForward 2 circle
        scores, (Deque.enqueueFront i circle')

let result = Seq.fold (insertMarble numPlayers) (Map.empty, Deque.empty) (Seq.init (numMarbles + 1) id)

printfn "%A" (result |> fst |> Map.toSeq |> Seq.maxBy snd)
