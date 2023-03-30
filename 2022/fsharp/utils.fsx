// vim: set et ts=4 sw=4 list :

open System.Collections.Generic 
open System.Text.RegularExpressions

let doProcess parseInput run rawInput =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let result =
        rawInput
        |> parseInput
        |> run

    stopwatch.Stop()
    printfn "Elapsed milliseconds: %f" stopwatch.Elapsed.TotalMilliseconds
    printfn "Result: %A" result

// Input utilities.

// Shorthand.
let readLines = System.IO.File.ReadLines

// Convert a string containing a binary number to its equivalent integer.
let binaryToInt s = System.Convert.ToInt32(s, 2)

// Turn a string containing a comma-separated list of numbers
// into an actual list of integers.
let commaInts (s : string) = s.Split(",") |> Seq.map int

// Given a list of strings (i.e. a 2-D list of characters), call f
// with each row, column, and character.
// Note to self: have f return an option and pipe into `Seq.choose id` if not all are needed.
let indexMapCharacters f input =
    input
    |> Seq.map Seq.indexed
    |> Seq.indexed
    |> Seq.collect (fun (r, rs) -> rs |> Seq.map (fun (c, x) -> f r c x))


// Partial active pattern to match on regex.
// Usage:
// match foo with
//     | Regex "someRegex(with)capturing(parens)" [group1; group2] -> something
// ...
// Borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None


// Memoize a function.
let memoize (f : 'a -> 'b) =
    let dict = new Dictionary<'a, 'b>()

    let memoizedFunc (input : 'a) =
        match dict.TryGetValue(input) with
        | true, x -> x
        | false, _ ->
            let answer = f input
            dict.Add(input, answer)
            answer

    memoizedFunc


// Given a predicate and a sequence, split the sequence into subsequences
// between elements that match the predicate.
// Borrowed from https://stackoverflow.com/questions/6736464/split-seq-in-f
let splitSeq p s =
    let i = ref 0
    s
    |> Seq.map (fun x ->
        if p x then i.Value <- i.Value + 1
        i.Value, x)
    |> Seq.filter (fun (i, x) -> (not << p) x)
    |> Seq.groupBy fst
    |> Seq.map (fun (_, b) -> Seq.map snd b)



// borrowed from https://stackoverflow.com/questions/1526046/f-permutations
let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)




let mapInvert map = map |> Map.toSeq |> Seq.map (fun (a, b) -> (b, a)) |> Map.ofSeq

let mapValues map = map |> Map.toList |> List.map snd

let consolidateDigits ds =
    let rec go total = function
    | [] -> total
    | h :: t -> go (total * 10 + h) t

    go 0 (List.ofSeq ds)


module Deque =
    type Deque<'a> = Empty | Deque of 'a list  * 'a list

    let empty = Empty

    let ofSeq xs = Deque ([], List.ofSeq xs)

    let toList = function
        | Empty -> []
        | Deque (back, front) -> front @ (List.rev back)

    let dump = function
        | Empty -> "[empty deque]"
        | Deque (back, front) -> sprintf "[deque: %A]" (front @ (List.rev back))

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
                elif (n - 1) < List.length xs then Deque ((xs |> List.take (n - 1) |> List.rev) @ (x :: back), List.skip (n - 1) xs)
                else rotateForward (n - (List.length xs + 1)) (Deque ([], (List.rev back) @ (x :: xs)))

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
                elif (n - 1) < List.length xs then Deque (List.skip (n - 1) xs, (xs |> List.take (n - 1) |> List.rev) @ (x :: front))
                else rotateBack (n - (List.length xs + 1)) (Deque ((List.rev front) @ (x :: xs), []))

    let rotateTo item deque =
        match deque with
        | Empty -> Empty
        | Deque (back, front) ->
            match List.tryFindIndex ((=) item) front with
            | Some index -> deque |> rotateForward index
            | None ->
                match List.tryFindIndex ((=) item) back with
                | Some index -> deque |> rotateBack (index + 1)
                | None -> deque
