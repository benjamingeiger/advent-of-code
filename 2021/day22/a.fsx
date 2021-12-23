// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let input = readLines "input.txt" |> List.ofSeq

type Point = { x : int; y : int; z : int }
type Cuboid = { low : Point; high : Point }
type Command = On of Cuboid | Off of Cuboid

let parseInput input =
    input
    |> Seq.choose (function
        | Regex "^(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)$" [command; xmin; xmax; ymin; ymax; zmin; zmax] ->
            match command with
            | "on" -> Some (On { low = { x = int xmin; y = int ymin; z = int zmin }; high = { x = int xmax; y = int ymax; z = int zmax } })
            | "off" -> Some (Off { low = { x = int xmin; y = int ymin; z = int zmin }; high = { x = int xmax; y = int ymax; z = int zmax } })
            | _ ->
                printfn "Invalid command '%s'" command
                None
        | x ->
            printfn "Unparseable input line '%s'" x
            None)

let containsPoint cuboid point =
    cuboid.low.x <= point.x && point.x <= cuboid.high.x && cuboid.low.y <= point.y && point.y <= cuboid.high.y && cuboid.low.z <= point.z && point.z <= cuboid.high.z

let containsCuboid outer inner =
    containsPoint outer inner.low && containsPoint outer inner.high

let isDisjointCuboid this that =
    this.high.x < that.low.x || that.high.x < this.low.x || this.high.y < that.low.y || that.high.y < this.low.y || this.high.z < that.low.z || that.high.z < this.low.z

let dumpCuboid c = sprintf "Cuboid (x=%d..%d, y=%d..%d, z=%d..%d)" c.low.x c.high.x c.low.y c.high.y c.low.z c.high.z

let splitCuboid outer inner =
    if isDisjointCuboid outer inner then [outer]
    else
        [outer]
        // split on low x plane
        |> List.collect (fun outer' ->
            if outer'.low.x < inner.low.x && inner.low.x <= outer'.high.x then
                [{ outer' with high = { outer'.high with x = inner.low.x - 1 }}; { outer' with low = { outer'.low with x = inner.low.x }}]
            else
                [ outer' ])
        // split on high x plane
        |> List.collect (fun outer' ->
            if outer'.low.x <= inner.high.x && inner.high.x < outer'.high.x then
                [{ outer' with high = { outer'.high with x = inner.high.x }}; { outer' with low = { outer'.low with x = inner.high.x + 1 }}]
            else
                [ outer' ])
        // split on low y plane
        |> List.collect (fun outer' ->
            if outer'.low.y < inner.low.y && inner.low.y <= outer'.high.y then
                [{ outer' with high = { outer'.high with y = inner.low.y - 1 }}; { outer' with low = { outer'.low with y = inner.low.y }}]
            else
                [ outer' ])
        // split on high y plane
        |> List.collect (fun outer' ->
            if outer'.low.y <= inner.high.y && inner.high.y < outer'.high.y then
                [{ outer' with high = { outer'.high with y = inner.high.y }}; { outer' with low = { outer'.low with y = inner.high.y + 1 }}]
            else
                [ outer' ])
        // split on low z plane
        |> List.collect (fun outer' ->
            if outer'.low.z < inner.low.z && inner.low.z <= outer'.high.z then
                [{ outer' with high = { outer'.high with z = inner.low.z - 1 }}; { outer' with low = { outer'.low with z = inner.low.z }}]
            else
                [ outer' ])
        // split on high z plane
        |> List.collect (fun outer' ->
            if outer'.low.z <= inner.high.z && inner.high.z < outer'.high.z then
                [{ outer' with high = { outer'.high with z = inner.high.z }}; { outer' with low = { outer'.low with z = inner.high.z + 1 }}]
            else
                [ outer' ])

let intersectCuboids this that =
    if isDisjointCuboid this that then None
    else
        let xs = Array.sort [| this.low.x; this.high.x; that.low.x; that.high.x |]
        let ys = Array.sort [| this.low.y; this.high.y; that.low.y; that.high.y |]
        let zs = Array.sort [| this.low.z; this.high.z; that.low.z; that.high.z |]

        Some { low = { x = xs.[1]; y = ys.[1]; z = zs.[1] }; high = { x = xs.[2]; y = ys.[2]; z = zs.[2] } }

let volume cuboid = (cuboid.high.x - cuboid.low.x + 1) * (cuboid.high.y - cuboid.low.y + 1) * (cuboid.high.z - cuboid.low.z + 1)

let compute space (i, command) =
    match command with
    | On cuboid ->
        cuboid :: space
    | Off cuboid ->
        space
        |> List.collect (fun c -> splitCuboid c cuboid)
        |> List.filter (fun rect -> not (containsCuboid cuboid rect))

let addVolumes space =
    let rec step total = function
        | [] -> total
        | cuboid :: rest ->
            let newTotal = total + volume cuboid
            let newSpace =
                rest
                |> List.collect (fun c ->
                    if isDisjointCuboid cuboid c then [c]
                    else
                        splitCuboid c cuboid |> List.filter (fun c' -> not (containsCuboid cuboid c')))

            step newTotal newSpace

    step 0 (space |> Set.ofList |> Set.toList)

let isInitialization cuboid =
    cuboid.low.x >= -50 && cuboid.low.y >= -50 && cuboid.low.z >= -50 && cuboid.high.x <= 50 && cuboid.high.y <= 50 && cuboid.high.z <= 50

let result =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let parsed =
        parseInput input
        |> Seq.filter (function | On cuboid | Off cuboid -> isInitialization cuboid)

    let returnValue =
        Seq.fold compute [] (Seq.indexed parsed)
        |> addVolumes

    stopwatch.Stop()
    printfn "Elapsed milliseconds: %f" stopwatch.Elapsed.TotalMilliseconds

    returnValue

printfn "%A" result
