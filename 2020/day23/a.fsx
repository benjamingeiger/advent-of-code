// vim: set et ts=4 sw=4 list :

open System
open System.Collections.Generic 

let enumerateSeq s = Seq.zip (Seq.initInfinite id) s

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
let input = readLines "input.txt" |> Seq.head |> Seq.toList |> List.map (fun c -> (int c) - (int '0'))

let step cups =
    let rotate circle =
        match circle with
        | [] -> []
        | h :: t -> t @ [h]

    let rec remove_cups circle =
        match circle with
        | [] -> failwith "but how?"
        | h :: r1 :: r2 :: r3 :: t -> ([r1; r2; r3], h :: t)
        | _ -> failwith "this shouldn't happen"

    let rec insert_after destination pickedup circle =
        match circle with
        | [] -> failwith "ran out of cups, whoops"
        | h :: t when h = destination -> (h :: pickedup) @ t
        | h :: t -> h :: (insert_after destination pickedup t)

    let rec immediately_before (previous : int) circle =
        if previous = 1 then immediately_before ((List.max circle) + 1) circle
        else if (List.contains (previous - 1) circle) then (previous - 1)
        else immediately_before (previous - 1) circle


    cups
    |> remove_cups
    |> fun (removed, remaining) -> insert_after (immediately_before (List.head remaining) remaining) removed remaining
    |> rotate

    (*Vlet (removed, remaining) = remove_cups input*)
    (*insert_after 2 removed remaining*)

printfn "%A" input
printfn "%A" (step input)
printfn "%A" (step (step input))

let rec generate f x count =
    if count = 0 then []
    else
        let cur = f x
        cur :: (generate f cur (count - 1))

let result = (generate step input 100)

let rec rotate_to target cups =
    match cups with
    | [] -> failwith "no. just no."
    | h :: t when h = target -> h :: t
    | h :: t -> rotate_to target (t @ [h])

let answer =
    result
    |> List.last
    |> rotate_to 1
    |> List.tail
    |> List.map string
    |> String.concat ""
    |> printfn "%s"
