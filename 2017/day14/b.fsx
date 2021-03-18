// vim: set et ts=4 sw=4 list :

open System

let input = "jxqlasbh"

let generateHash key =
    let generateInput key =
        key
        |> Seq.map int
        |> Array.ofSeq
        |> fun a -> Array.concat [|a; [| 17; 31; 73; 47; 23 |] |]
        |> Array.replicate 64
        |> Array.concat

    let flipStartOfArray n a =
        Array.permute (fun i -> if i < n then n - i - 1 else i) a

    let rotateArray n a =
        let l = Array.length a
        Array.permute (fun i -> (l + i - n) % l) a

    let rotateStartingAt curPos length a =
        a
        |> rotateArray curPos
        |> flipStartOfArray length
        |> rotateArray -curPos

    let step (a, curPos, skipSize) length =
        let newA = rotateStartingAt curPos length a
        let newCurPos = (curPos + skipSize + length) % (Array.length a)
        let newSkipSize = skipSize + 1

        (newA, newCurPos, newSkipSize)

    key
    |> generateInput
    |> Seq.fold step ((Array.init 256 id), 0, 0)
    |> fun (a, _, _) -> a
    |> Array.chunkBySize 16
    |> Array.map (Array.fold (^^^) 0)
    |> Array.map (sprintf "%02x")
    |> String.concat ""

let createGrid hexstr =
    hexstr
    |> Seq.map (function
        | '0' -> "...." | '1' -> "...#" | '2' -> "..#." | '3' -> "..##"
        | '4' -> ".#.." | '5' -> ".#.#" | '6' -> ".##." | '7' -> ".###"
        | '8' -> "#..." | '9' -> "#..#" | 'a' -> "#.#." | 'b' -> "#.##"
        | 'c' -> "##.." | 'd' -> "##.#" | 'e' -> "###." | 'f' -> "####"
        | _ -> "")
    |> String.concat ""

let cells =
    [0..127]
    |> List.map (sprintf "%s-%d" input)
    |> List.map generateHash
    |> List.map createGrid
    |> List.map Seq.indexed
    |> List.map (Seq.choose (fun (c, x) -> if x = '#' then Some c else None))
    |> List.indexed
    |> List.map (fun (r, cs) -> cs |> Seq.map (fun c -> (r, c)))
    |> Seq.concat
    |> Set.ofSeq

let bfs cells origin =
    let neighbors (r, c) =
        [(r + 1, c); (r - 1, c); (r, c + 1); (r, c - 1)]
        |> List.filter (fun x -> Set.contains x cells)

    let rec step visited queue =
        let cur = List.head queue
        let potentialNeighbors =
            cur
            |> neighbors
            |> List.filter (fun n -> not (Set.contains n visited || List.contains n queue))
        let newQueue = List.append (List.tail queue) potentialNeighbors

        if List.isEmpty newQueue then Set.add cur visited else step (Set.add cur visited) newQueue

    step Set.empty [origin]

let countRegions cells =
    let rec go count remainingCells =
        if Set.isEmpty remainingCells then count
        else
            let currentRegion = bfs remainingCells (Set.minElement remainingCells)

            go (count + 1) (Set.difference remainingCells currentRegion)

    go 0 cells

let result = countRegions cells

printfn "%A" result

