// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt"

type Operation =
    | SwapPosition of int * int
    | SwapCharacter of char * char
    | RotateLeft of int
    | RotateRight of int
    | RotateBasedOn of char
    | Reverse of int * int
    | Move of int * int

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parseLine = function
    | Regex "swap position (\d+) with position (\d+)" [f; t] -> SwapPosition (int f, int t)
    | Regex "swap letter (\w) with letter (\w)" [f; t] -> SwapCharacter (f.[0], t.[0])
    | Regex "rotate left (\d+) step" [n] -> RotateLeft (int n)
    | Regex "rotate right (\d+) step" [n] -> RotateRight (int n)
    | Regex "rotate based on position of letter (\w)" [c] -> RotateBasedOn (c.[0])
    | Regex "reverse positions (\d+) through (\d+)" [f; t] -> Reverse (int f, int t)
    | Regex "move position (\d+) to position (\d+)" [f; t] -> Move (int f, int t)
    | x -> failwith (sprintf "Invalid command [%A]" x)

let instructions = input |> Seq.map parseLine

let scramble instructions password =
    let buffer = Array.ofSeq password
    let alternate = Array.copy buffer
    let passwordLength = Array.length buffer

    let indexOf a v = Array.findIndex ((=) v) a

    // rotates the elements in buffer between f and t right by n
    let rotateBuffer buffer (f, t) n =
        let n' = n % (t - f + 1)
        Array.blit buffer f alternate (f + n') ((t - f + 1) - n')
        Array.blit buffer (t - n' + 1) alternate f n'

        Array.blit alternate f buffer f (t - f + 1)

        buffer

    let rec perform (buffer : char[]) inst = 
        match inst with
        | SwapPosition (f, t) ->
            let tmp = buffer.[f]
            buffer.[f] <- buffer.[t]
            buffer.[t] <- tmp

            buffer

        | SwapCharacter (f, t) ->
            let f' = indexOf buffer f
            let t' = indexOf buffer t

            perform buffer (SwapPosition (f', t'))

        | RotateLeft n ->
            rotateBuffer buffer (0, passwordLength - 1) (passwordLength - n)

        | RotateRight n ->
            rotateBuffer buffer (0, passwordLength - 1) n

        | RotateBasedOn c ->
            let initPos = indexOf buffer c

            rotateBuffer buffer (0, passwordLength - 1) (if initPos >= 4 then initPos + 2 else initPos + 1)

        | Reverse (f, t) ->
            Array.blit (Array.rev (Array.sub buffer f (t - f + 1))) 0 buffer f (t - f + 1)

            buffer

        | Move (f, t) ->
            if f < t
            then rotateBuffer buffer (f, t) (t - f)
            else rotateBuffer buffer (t, f) 1

            buffer

    Seq.fold perform buffer instructions

// Borrowed from http://www.fssnip.net/4u/title/Very-Fast-Permutations
let rec permutations = function
    | []      -> seq [List.empty]
    | x :: xs -> Seq.collect (insertions x) (permutations xs)
and insertions x = function
    | []              -> [[x]]
    | (y :: ys) as xs -> (x::xs)::(List.map (fun x -> y::x) (insertions x ys))

let result =
    permutations (List.ofSeq "fbgdceah")
    |> Seq.map (fun p -> (p, scramble instructions p))
    |> Seq.filter (fun (p, h) -> h = [| 'f'; 'b'; 'g'; 'd'; 'c'; 'e'; 'a'; 'h' |])
    |> Seq.take 1

printfn "%A" (fst result)

