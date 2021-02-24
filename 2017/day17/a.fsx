// vim: set et ts=4 sw=4 list :

open System

let input = 371
(*let input = 3*)

module Deque =
    type Deque<'a> = Empty | Deque of 'a list  * 'a list

    let empty = Empty

    let private flip = function
        | Empty -> Empty
        | Deque ([], front) -> Deque (List.rev front, [])
        | Deque (back, []) -> Deque ([], List.rev back)
        | Deque (back, front) -> Deque (back, front)

    let size = function
        | Empty -> 0
        | Deque (back, front) -> List.length back + List.length front

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
        if n >= (size deque) && (size deque > 0)
        then rotateForward (n % size deque) deque
        else
            match deque with
            | Empty -> Empty
            | Deque (back, []) -> rotateForward n (flip deque)
            | Deque (back, x :: xs) ->
                if n = 0 then Deque (back, x :: xs)
                elif n = 1 then Deque (x :: back, xs)
                else rotateForward (n - 1) (Deque (x :: back, xs))

    let rec rotateBack n deque =
        if n >= (size deque) && (size deque) > 0
        then rotateBack (n % size deque) deque
        else
            match deque with
            | Empty -> Empty
            | Deque ([], front) -> rotateBack n (flip deque)
            | Deque (x :: xs, front) ->
                if n = 0 then Deque (x :: xs, front)
                elif n = 1 then Deque (xs, x :: front)
                else rotateBack (n - 1) (Deque (xs, x :: front))

let insert offset deque value =
    deque
    |> Deque.rotateForward offset
    |> Deque.enqueueBack value

let result = [0..2017] |> List.fold (insert input) Deque.empty

printfn "%A" (Deque.dequeueFront result)
