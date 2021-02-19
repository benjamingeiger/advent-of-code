// vim: set et ts=4 sw=4 list :

open System

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.map List.ofSeq |> List.ofSeq

// LeftTurn and RightTurn are relative to someone traveling north/south
type Cell = Vertical | Horizontal | LeftCurve | RightCurve | Intersection | Empty

type Direction = Up | Right | Down | Left
type Turn = LeftTurn | Straight | RightTurn

let nextTurn = function
    | LeftTurn -> Straight
    | Straight -> RightTurn
    | RightTurn -> LeftTurn

type Cart = Cart of Direction * (int * int) * Turn

let parseMap (input : char list list) =
    let rowCount = Seq.length input
    let colCount = Seq.length (Seq.head input)

    printfn "%A %A" rowCount colCount

    let map = Array2D.init colCount rowCount (fun x y ->
        (*printfn "%A %A %A" y x*)
        (*printfn "%A" input.[y].[x]*)
        match input.[y].[x] with
        | '|' -> Vertical
        | '-' -> Horizontal
        | '\\' -> LeftCurve
        | '/' -> RightCurve
        | '+' -> Intersection
        | '^' -> Vertical
        | 'v' -> Vertical
        | '<' -> Horizontal
        | '>' -> Horizontal
        | _ -> Empty)

    let carts =
        input
        |> List.mapi (fun y cs ->
            cs
            |> List.mapi (fun x c ->
                match c with
                | '^' -> Some (Cart (Up, (x, y), LeftTurn))
                | '>' -> Some (Cart (Right, (x, y), LeftTurn))
                | 'v' -> Some (Cart (Down, (x, y), LeftTurn))
                | '<' -> Some (Cart (Left, (x, y), LeftTurn))
                | _ -> None)
            |> List.choose id)
        |> List.collect id

    map, carts

let (map, carts) = parseMap input

let tick (map : Cell [,]) carts =
    let updateCart = function
        | Cart (Up, (x, y), turn) ->
            match map.[x, y] with
            | Vertical -> Cart (Up, (x, y - 1), turn)
            | Horizontal -> failwith "moving up on horizontal track"
            | LeftCurve -> Cart (Left, (x - 1, y), turn)
            | RightCurve -> Cart (Right, (x + 1, y), turn)
            | Intersection ->
                match turn with
                | LeftTurn -> Cart (Left, (x - 1, y), nextTurn turn)
                | Straight -> Cart (Up, (x, y - 1), nextTurn turn)
                | RightTurn -> Cart (Right, (x + 1, y), nextTurn turn)
            | Empty -> failwith (sprintf "cart derailed Up %A" (x, y))

        | Cart (Right, (x, y), turn) ->
            match map.[x, y] with
            | Vertical -> failwith "moving right on vertical track"
            | Horizontal -> Cart (Right, (x + 1, y), turn)
            | LeftCurve -> Cart (Down, (x, y + 1), turn)
            | RightCurve -> Cart (Up, (x, y - 1), turn)
            | Intersection ->
                match turn with
                | LeftTurn -> Cart (Up, (x, y - 1), nextTurn turn)
                | Straight -> Cart (Right, (x + 1, y), nextTurn turn)
                | RightTurn -> Cart (Down, (x, y + 1), nextTurn turn)
            | Empty -> failwith (sprintf "cart derailed Right %A" (x, y))

        | Cart (Down, (x, y), turn) ->
            match map.[x, y] with
            | Vertical -> Cart (Down, (x, y + 1), turn)
            | Horizontal -> failwith "moving down on horizontal track"
            | LeftCurve -> Cart (Right, (x + 1, y), turn)
            | RightCurve -> Cart (Left, (x - 1, y), turn)
            | Intersection ->
                match turn with
                | LeftTurn -> Cart (Right, (x + 1, y), nextTurn turn)
                | Straight -> Cart (Down, (x, y + 1), nextTurn turn)
                | RightTurn -> Cart (Left, (x - 1, y), nextTurn turn)
            | Empty -> failwith (sprintf "cart derailed Down %A" (x, y))

        | Cart (Left, (x, y), turn) ->
            match map.[x, y] with
            | Vertical -> failwith "moving left on vertical track"
            | Horizontal -> Cart (Left, (x - 1, y), turn)
            | LeftCurve -> Cart (Up, (x, y - 1), turn)
            | RightCurve -> Cart (Down, (x, y + 1), turn)
            | Intersection ->
                match turn with
                | LeftTurn -> Cart (Down, (x, y + 1), nextTurn turn)
                | Straight -> Cart (Left, (x - 1, y), nextTurn turn)
                | RightTurn -> Cart (Up, (x, y - 1), nextTurn turn)
            | Empty -> failwith (sprintf "cart derailed Left %A" (x, y))

    let collisions carts =
        let crashLocs = 
            carts
            |> List.groupBy (function | Cart (_, (x, y), _) -> (x, y))
            |> List.choose (fun (k, v) -> match v.Length with | x when x > 1 -> Some k | _ -> None)
            |> Set.ofList

        List.partition (function | Cart (_, (x, y), _) -> Set.contains (x, y) crashLocs) carts

    let rec go uncrashed toProcess =
        match toProcess with
        | [] -> Ok (List.rev uncrashed)
        | cur :: rest ->
            let updated = updateCart cur
            let (crashed, uncrashed') = collisions (List.concat [uncrashed; [updated]; rest])

            if List.isEmpty crashed
            then
                go (updated :: uncrashed) rest
            else Error crashed

    carts
    |> List.sortBy (function | Cart (_, (x, y), _) -> (y, x))
    |> go []

let rec findCrash map carts =
    printfn "%A" carts
    match tick map carts with
    | Ok carts' -> findCrash map carts'
    | Error crashLoc -> crashLoc

let crashLoc = findCrash map carts
printfn "%A" crashLoc

