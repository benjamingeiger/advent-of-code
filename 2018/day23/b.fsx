// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readLines filePath = System.IO.File.ReadLines filePath
let input =
    readLines "input.txt"
    |> Seq.choose (function
        | Regex @"^pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)$" [x; y; z; r] ->
            Some ((int x, int y, int z), int r)

        | s ->
            eprintfn "invalid bot [%A]" s
            None)
    |> Seq.toList

let solve bots =

    let center ((xl, yl, zl), (xh, yh, zh)) =
        ((xl + (xh - xl) / 2), (yl + (yh - yl) / 2), (zl + (zh - zl) / 2))

    let splitRect ((xl, yl, zl), (xh, yh, zh)) =
        let (xm, ym, zm) = center ((xl, yl, zl), (xh, yh, zh))

        [ ((xl    , yl    , zl    ), (xm, ym, zm))
          ((xm + 1, yl    , zl    ), (xh, ym, zm))
          ((xl    , ym + 1, zl    ), (xm, yh, zm))
          ((xm + 1, ym + 1, zl    ), (xh, yh, zm))
          ((xl    , yl    , zm + 1), (xm, ym, zh))
          ((xm + 1, yl    , zm + 1), (xh, ym, zh))
          ((xl    , ym + 1, zm + 1), (xm, yh, zh))
          ((xm + 1, ym + 1, zm + 1), (xh, yh, zh)) ]

    let maxDim =
        let minPowerOf2Above n = let rec go x = if x > n then x else go (2 * x) in go 1

        bots 
        |> List.collect (fun ((x, y, z), r) ->
            [x + r; x - r; y + r; y - r; z + r; z - r]
            |> List.map abs)
        |> Seq.max
        |> minPowerOf2Above

    let initRect = ((-maxDim, -maxDim, -maxDim), (maxDim - 1, maxDim - 1, maxDim - 1))

    let manhattan (x1, y1, z1) (x2, y2, z2) = abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)

    let centerDistance = center >> manhattan (0, 0, 0)

    let intersects ((xl, yl, zl), (xh, yh, zh)) ((bx, by, bz), br) =
        let distance n l h =
            if n < l then abs (n - l)
            elif n > h then abs (n - h)
            else 0

        (distance bx xl xh + distance by yl yh + distance bz zl zh) <= br

    let intersectCount ((xl, yl, zl), (xh, yh, zh)) =
        bots
        |> List.filter (intersects ((xl, yl, zl), (xh, yh, zh)))
        |> List.length

    let rec step pq =
        let (key, cur) = Set.minElement pq
        let pq' = Set.remove (key, cur) pq

        let ((xl, yl, zl), (xh, yh, zh)) = cur
        if xl = xh && yl = yh && zl = zh then
            centerDistance cur
        else
            let pq'' =
                splitRect cur
                |> List.map (fun cur' -> ((-1 * intersectCount cur', centerDistance cur'), cur'))
                |> Set.ofList
                |> Set.union pq'

            step pq''


    step (Set.singleton ((-1 * List.length bots, centerDistance initRect), initRect))

let result = solve input
printfn "%A" result
