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

let bfs passcode origin destination =
    let rec step queue =
        let cur = List.head queue
        let newQueue = List.append (List.tail queue) (neighbors cur)

        if (snd cur) = destination then fst cur else step newQueue

    step [(passcode, origin)]

printfn "%A" (bfs input (0, 0) (3, 3))
