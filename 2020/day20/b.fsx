// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic 

// borrowed from https://stackoverflow.com/questions/43574032/how-does-this-transpose-function-work
let rec transposeList = function
| [] -> failwith "cannot transpose a 0-by-n matrix"
| []::xs -> [] 
| xs -> List.map List.head xs :: transposeList (List.map List.tail xs)

let enumerateSeq s = Seq.zip (Seq.initInfinite id) s

// borrowed from https://stackoverflow.com/questions/6736464/split-seq-in-f
let splitSeq p s =
    let i = ref 0
    s
    |> Seq.map (fun x ->
        if p x then incr i
        !i, x)
    |> Seq.filter (fun (i, x) -> (not << p) x)
    |> Seq.groupBy fst
    |> Seq.map (fun (_, b) -> Seq.map snd b)

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

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> splitSeq ((=) "") |> Seq.map Seq.toList |> Seq.toList

let invert (s : string) = (s.ToCharArray() |> Array.rev |> System.String)

type Operation = Rotate | Flip

let getRawImage (text : string list) =
    let header = List.head text
    let number = (header.Split " ").[1] |> (fun l -> int l.[..(String.length l) - 2])
    let scanLines = List.tail text

    (number, scanLines)

let getPiece (number, scanLines) =
    let top = List.head scanLines
    let bottom = invert (List.last scanLines)
    let left = invert (String.concat "" (List.map (fun (l : string) -> string l.[0]) scanLines))
    let right = String.concat "" (List.map (fun (l : string) -> string l.[String.length l - 1]) scanLines)

    (number, top, right, bottom, left, [])

let stripBorder (number, scanLines) =
    let pixels =
        scanLines
        |> List.tail 
        |> List.rev 
        |> List.tail 
        |> List.rev 
        |> List.map (fun (s : string) -> s.[1..(String.length s) - 2])
        |> List.map List.ofSeq

    (number, pixels)

let rawImages = input |> List.map getRawImage
let pieces = rawImages |> List.map getPiece
let images = rawImages |> List.map stripBorder |> Map.ofList

let pieceCount = List.length pieces
let gridSize = match pieceCount with 9 -> 3 | 144 -> 12 | _ -> failwith "screw it, wrong input size"

let rotate (number, top, right, bottom, left, ops) = number, left, top, right, bottom, Rotate :: ops

let flip (number, top, right, bottom, left, ops) = (number, invert left, invert bottom, invert right, invert top, Flip :: ops)

let canonical edge =
    let bits (edge : string) =
        let rec bits' acc cs =
            match cs with
            | [] -> acc
            | h :: t -> bits' (acc * 2 + (if h = "#" then 1 else 0)) t

        bits' 0 (edge.ToCharArray() |> Array.toList |> List.map string)

    let forward = bits edge
    let reverse = bits (invert edge)

    if forward > reverse then edge else invert edge

// let's try this one first
let allEdgePatterns pieces =
    pieces 
    |> List.collect (fun (_, t, r, b, l, o) -> [t; r; b; l])
    |> List.map canonical
    |> List.sort
    |> List.rev
    |> Seq.groupBy id
    |> Seq.map (fun (k, v) -> k, (Seq.length v))

let unmatchedEdges =
    allEdgePatterns pieces
    |> Seq.filter (fun (k, c) -> c = 1)
    |> Seq.map fst
    |> Set.ofSeq

let isCornerPiece (number, top, right, bottom, left, ops) =
    [top; right; bottom; left]
    |> List.map canonical
    |> List.filter (fun e -> Set.contains e unmatchedEdges)
    |> fun x -> List.length x = 2

let cornerPieces = pieces |> List.filter isCornerPiece

let matchToLeft numbers pattern =
    let orient ((number, top, right, bottom, left, ops) as piece) =
        if Set.contains number numbers then None
        else if (invert left) = pattern then 
            Some piece
        else if (invert bottom) = pattern then
            Some (rotate piece)
        else if (invert right) = pattern then
            Some (rotate (rotate piece))
        else if (invert top) = pattern then
            Some (rotate (rotate (rotate piece)))
        else if top = pattern then
            Some (flip piece)
        else if right = pattern then
            Some (rotate (flip piece))
        else if bottom = pattern then 
            Some (rotate (rotate (flip piece)))
        else if left = pattern then 
            Some (rotate (rotate (rotate (flip piece))))
        else None

    pieces
    |> Seq.map orient
    |> Seq.find Option.isSome

let matchToTop numbers pattern =
    match matchToLeft numbers pattern with
    | None -> None
    | Some piece -> Some (rotate piece)

let buildGrid size ((n, t, r, b, l, o) as firstCorner) =
    let rec buildColumn numbers col =
        if List.length col = size then List.rev col
        else
            let (number, _, _, pattern, _, _) = List.head col
            match matchToTop (Set.add number numbers) pattern with
            | None -> failwith "so much for that idea"
            | Some piece -> buildColumn (Set.add number numbers) (piece :: col)

    let rec buildRow numbers row =
        if List.length row = size then List.rev row
        else
            let (number, _, pattern, _, _, _) = List.head row
            match matchToLeft (Set.add number numbers) pattern with
            | None -> failwith "so much for that idea"
            | Some piece -> buildRow (Set.add number numbers) (piece :: row)

    buildColumn (Set.singleton n) [firstCorner]
    |> List.map (fun ((n', t', r', b', l', o') as p) -> buildRow (Set.singleton n') [p])

let firstPiece =
    let rec orient ((number, top, right, bottom, left, ops) as piece) =
        if Set.contains (canonical top) unmatchedEdges && Set.contains (canonical left) unmatchedEdges
        then piece
        else orient (rotate piece)

    cornerPieces
    |> List.head
    |> orient

let fullGrid =
    firstPiece
    |> buildGrid gridSize

let rotateImage scanLines =
    transposeList scanLines |> List.map List.rev

let flipImage scanLines =
    transposeList scanLines

let rec orientImage scanLines ops =
    match ops with
    | [] -> scanLines
    | Rotate :: t -> orientImage (rotateImage scanLines) t
    | Flip :: t -> orientImage (flipImage scanLines) t

let retrieveImage images (n, _, _, _, _, ops) =
    match Map.tryFind n images with
    | None -> failwith "lolwtf"
    | Some scanLines ->
        orientImage scanLines (List.rev ops)

let joinImageRow scanLines1 scanLines2 =
    List.zip scanLines1 scanLines2
    |> List.map (fun (a, b) -> List.concat [a; b])

let buildImageRow row =
    row
    |> List.map (retrieveImage images)
    |> List.reduce joinImageRow

let buildImage grid =
    grid
    |> List.map buildImageRow
    |> List.concat
    (*|> List.map (List.map string)*)
    (*|> List.map (String.concat "")*)

let fullImage = buildImage fullGrid

let monsterAt r c =
    [(r + 0, c + 18);
    (r + 1, c + 0);
    (r + 1, c + 5);
    (r + 1, c + 6);
    (r + 1, c + 11);
    (r + 1, c + 12);
    (r + 1, c + 17);
    (r + 1, c + 18);
    (r + 1, c + 19);
    (r + 2, c + 1);
    (r + 2, c + 4);
    (r + 2, c + 7);
    (r + 2, c + 10);
    (r + 2, c + 13);
    (r + 2, c + 16)]

let findMonster (image : char list list) =
    let atPosition r c =
        let monsterPos = monsterAt r c
        if (Seq.forall (fun (r, c) -> image.[r].[c] = '#') monsterPos) then monsterPos else []

    let rows = List.length image
    let cols = List.length (List.head image)

    [ for r in [0..(rows - 3)] do for c in [0..(rows - 20)] do yield! atPosition r c ]

let variants = 
    [fullImage;
    rotateImage fullImage;
    rotateImage (rotateImage fullImage);
    rotateImage (rotateImage (rotateImage fullImage));
    flipImage fullImage;
    rotateImage (flipImage fullImage);
    rotateImage (rotateImage (flipImage fullImage));
    rotateImage (rotateImage (rotateImage (flipImage fullImage)))]

let (positiveImage, monsters) = 
    variants
    |> List.map (fun v -> (v, findMonster v))
    |> List.filter (fun (v, r) -> List.length r > 0)
    |> List.exactlyOne

let waveSquares = positiveImage |> List.map (List.filter ((=) '#')) |> List.map (List.length) |> List.reduce (+)

let monsterSquares = monsters |> List.sort |> List.distinct |> List.length

printfn "%d %d %d" monsterSquares waveSquares (waveSquares - monsterSquares)

