// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

type SimResult = Overshot | Undershot | Fallthrough | Hit of int * int

let input = readLines "input.txt" |> List.ofSeq

let parseInput input =
    input
    |> List.head
    |> function
        | Regex "^target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)$" [xmin; xmax; ymin; ymax] ->
            (int xmin, int xmax, int ymin, int ymax)
        | _ -> failwith "wtf"

let stepPhysics (vx, vy, sx, sy) =
    ((if vx = 0 then 0 else vx - (vx / abs vx)), vy - 1, sx + vx, sy + vy)

let simulate (xmin, xmax, ymin, ymax) (vx0, vy0) =
    let inTarget (sx, sy) = sx >= xmin && sx <= xmax && sy >= ymin && sy <= ymax

    let rec step (vx, vy, sx, sy) =
        let (vx', vy', sx', sy') = stepPhysics (vx, vy, sx, sy)

        if inTarget (sx, sy) then
            Hit (vx0, vy0)
        elif sy < ymin then
            if sx < xmin then Undershot
            elif sx > xmax then Overshot
            else Fallthrough
        else
            step (vx', vy', sx', sy')

    step (vx0, vy0, 0, 0)

let findSolutions (xmin, xmax, ymin, ymax) =
    // Yeah, this is spitballing but I don't care.
    // And I knew roughly what the max vy should be from part 1.
    let vxs = [0..xmax]
    let vys = [ymin..100]

    List.allPairs vxs vys
    |> List.map (simulate (xmin, xmax, ymin, ymax))
    |> List.choose (function
        | Hit (vx, vy) -> Some (vx, vy)
        | _ -> None)

let result =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let (xmin, xmax, ymin, ymax) = parseInput input

    printfn "Target range: %A" (xmin, xmax, ymin, ymax)

    let returnValue = (xmin, xmax, ymin, ymax) |> findSolutions |> List.length

    stopwatch.Stop()
    printfn "Elapsed milliseconds: %f" stopwatch.Elapsed.TotalMilliseconds

    returnValue

printfn "%A" result
