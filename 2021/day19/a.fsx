// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> List.ofSeq

let parseInput input =
    input
    |> splitSeq ((=) "")
    |> Seq.map Seq.tail
    |> Seq.map (Seq.choose (function
        | Regex "^(-?\d+),(-?\d+),(-?\d+)$" [x; y; z] ->
            Some (int x, int y, int z)
        | x ->
            printfn "Unparseable line: '%s'" x
            None))
    |> Seq.map List.ofSeq
    |> List.ofSeq


let allRotations =
    let aboutX (x, y, z) = (x, z, -y)
    let aboutY (x, y, z) = (-z, y, x)
    let aboutZ (x, y, z) = (y, -x, z)

    [
        id;
        aboutX;
        aboutY;
        aboutZ;
        aboutX >> aboutX;
        aboutX >> aboutY;
        aboutX >> aboutZ;
        aboutY >> aboutX;
        aboutY >> aboutY;
        aboutZ >> aboutY;
        aboutZ >> aboutZ;
        aboutX >> aboutX >> aboutX;
        aboutX >> aboutX >> aboutY;
        aboutX >> aboutX >> aboutZ;
        aboutX >> aboutY >> aboutX;
        aboutX >> aboutY >> aboutY;
        aboutX >> aboutZ >> aboutZ;
        aboutY >> aboutX >> aboutX;
        aboutY >> aboutY >> aboutY;
        aboutZ >> aboutZ >> aboutZ;
        aboutX >> aboutX >> aboutX >> aboutY;
        aboutX >> aboutX >> aboutY >> aboutX;
        aboutX >> aboutY >> aboutX >> aboutX;
        aboutX >> aboutY >> aboutY >> aboutY
    ]

let allBeaconRotations scans = allRotations |> List.map (fun f -> List.map f scans)

(*let beaconMatch (x1, y1, z1) (x2, y2, z2) = (x1 - y1 = x2 - y2) && (x1 - z1 = x2 - z2) && (y1 - z1 = y2 - z2)*)

let beaconShift (offsetx, offsety, offsetz) (x, y, z) = (x - offsetx, y - offsety, z - offsetz)

let beaconOffset (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)
let beaconOffset2 (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

let attemptScannerMatch scans1 scans2 =
    let findBeaconDifferences scans =
        List.allPairs scans scans
        |> List.filter (fun (x, y) -> x <> y)
        |> List.map (fun ((x1, y1, z1), (x2, y2, z2)) -> ((x2 - x1, y2 - y1, z2 - z1), ((x1, y1, z1), (x2, y2, z2))))

    let scan1BeaconPairs = findBeaconDifferences scans1
    let scan2BeaconPairs = findBeaconDifferences scans2

    let scanResult =
        scan2BeaconPairs
        |> List.filter (fun (diff, (p1, p2)) -> scan1BeaconPairs |> List.map fst |> List.contains diff)
        |> List.allPairs scan1BeaconPairs
        |> List.choose (fun ((d1, (p11, p12)), (d2, (p21, p22))) ->
            if d1 <> d2 then None
            else Some (beaconOffset p11 p21))
        |> List.countBy id
        |> function
            | [] -> None
            | xs ->
                xs
                |> List.maxBy snd
                |> Some

    match scanResult with
    | None -> None
    | Some (probablePosition, countOfMatchingBeacons) ->
        printfn "%A" probablePosition
        printfn "%A" countOfMatchingBeacons

        if countOfMatchingBeacons >= 132 then
            Some (probablePosition, List.map (fun s -> beaconOffset2 probablePosition s) scans2)
        else
            None

let findMatchingScanners existingBeaconSets scanSet =
    existingBeaconSets
    |> List.choose (fun s ->
        scanSet
        |> List.choose (fun ss -> attemptScannerMatch s ss)
        |> function
            | [] -> None
            | [x] -> Some x
            | h :: t -> failwith "lolwtf")

let attachScanners init scanSets =
    let rec go (existingScanners, unmatched) scanSets =
        let existingBeaconSets = List.map snd existingScanners

        printfn "existing beacon sets: %A" existingBeaconSets

        scanSets
        |> List.fold (fun (existingScanners', unmatched') scanSet ->
            match findMatchingScanners existingBeaconSets scanSet with
            | scans :: rest ->
                printfn "found scanners: %A more" (List.length rest)
                (scans :: existingScanners', unmatched')
            | [] -> (existingScanners', scanSet :: unmatched')) (existingScanners, [])
        |> function
            | (scanners, []) -> scanners
            | (scanners, unmatched'') ->
                printfn "Current status: %A" scanners
                go (scanners, []) unmatched''

    go (init, []) scanSets

let result =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let parsed = parseInput input

    let beacons = List.head parsed

    let scanSets =
        parsed
        |> List.tail
        |> List.map allBeaconRotations

    let scanResults = attachScanners [(0, 0, 0), beacons] scanSets

    let returnValue =
        scanResults
        |> List.map (snd >> Set.ofList)
        |> Set.unionMany
        |> Set.count


    stopwatch.Stop()
    printfn "Elapsed milliseconds: %f" stopwatch.Elapsed.TotalMilliseconds

    returnValue

printfn "%A" result
