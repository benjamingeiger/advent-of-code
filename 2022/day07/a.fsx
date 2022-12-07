// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

type Line =
    | Chdir of string
    | FileSize of uint64 * string

let rawInput = readLines "input.txt" |> List.ofSeq

let parseInput rawInput =
    rawInput
    |> Seq.choose (function
        | Regex "\$ cd (.*)" [dir] ->
            Some (Chdir dir)
        | Regex "(\d+) (.*)" [size; name] ->
            Some (FileSize (uint64 size, name))
        | _ -> None)

let runStep (curdir, dirs) = function
    | Chdir dir ->
        if dir = "/" then ([], dirs)
        elif dir = ".." then (List.tail curdir, dirs)
        // the Map.add here is to make sure that every directory
        // shows up. otherwise directories with no files didn't
        // get counted with their child directories' sizes.
        // this cost me more than half an hour.
        else (dir :: curdir, (Map.add (dir :: curdir) 0UL dirs))
    | FileSize (size, name) ->
        let currentSize = dirs |> Map.tryFind curdir |> Option.defaultValue 0UL
        (curdir, Map.add curdir (currentSize + size) dirs)

let rec isSubdir parent child =
    if (List.length child < List.length parent) then false
    elif parent = child then true
    else isSubdir parent (List.tail child)

let totalSize dirs =
    let keys = Map.keys dirs
    
    let subdirs d = keys |> Seq.filter (isSubdir d)

    let totalSize' k =
        k
        |> subdirs
        |> Seq.map (fun k' -> Map.tryFind k' dirs |> Option.defaultValue 0UL)
        |> Seq.sum

    keys
    |> Seq.map (fun k -> k, totalSize' k)
    |> Map.ofSeq

let run input =
    let dirs = input |> Seq.fold runStep ([], Map.empty) |> snd

    totalSize dirs
    |> Map.filter (fun _ v -> v <= 100000UL)
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.sum

doProcess parseInput run rawInput
