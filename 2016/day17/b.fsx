// vim: set et ts=4 sw=4 list :

open System

// borrowed from http://www.fssnip.net/3D/title/MD5-hash
open System.Security.Cryptography
open System.Text

let md5 (input : string) : string =
    let data = System.Text.Encoding.ASCII.GetBytes input

    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string

let input = "njfxhljp"
(*let input = "ihgpwlah"*)

type Position = int * int
type State = string * Position

let findLocks key =
    md5 key
    |> Seq.take 4
    |> Seq.map (function 'b' | 'c' | 'd' | 'e' | 'f' -> true | _ -> false)
    |> Seq.toList

let neighbors (path, (r, c)) =
    [(path + "U", (r - 1, c));
     (path + "D", (r + 1, c));
     (path + "L", (r, c - 1));
     (path + "R", (r, c + 1))]
    |> List.zip (findLocks path)
    |> List.filter (fun (x, _) -> x)
    |> List.map snd
    |> List.filter (fun (_, (r, _)) -> r >= 0 && r <= 3)
    |> List.filter (fun (_, (_, c)) -> c >= 0 && c <= 3)


(*printfn "%A" (neighbors (input, (0, 0)))*)

let bogofs passcode origin destination =
    let rec step queue visited =
        if List.isEmpty queue then visited else
            let cur = List.head queue

            if (snd cur) = destination
            then step (List.tail queue) ((fst cur) :: visited)
            else step (List.append (List.tail queue) (neighbors cur)) visited

    step [(passcode, origin)] []

let result =
    bogofs input (0, 0) (3, 3)
    |> List.map String.length
    |> List.head
    |> fun n -> n - (String.length input)
    |> printfn "%A"
