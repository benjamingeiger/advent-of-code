// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readLines filePath = System.IO.File.ReadLines filePath
let (controlText :: statesText) =
    readLines "input.txt" 
    |> String.concat "/" 
    |> fun (s: string) -> s.Split("//") 
    |> Array.toList

(*printfn "%A" controlText*)
(*printfn "%A" statesText*)

let (initialState, diagnosticSteps) =
    match controlText with
    | Regex "Begin in state ([A-Z])./Perform a diagnostic checksum after (\d+) steps."
            [initialState; diagnosticStepsText] -> initialState, (int diagnosticStepsText)
    | _ -> failwithf "Invalid control stanza [%A]" controlText

(*printfn "%A %A" initialState diagnosticSteps*)

type Direction = Left | Right

let parseState stateText =
    let headerRegex = "In state ([A-Z]):"
    let currentValueRegex = "  If the current value is (\d+):"
    let newValueRegex = "    - Write the value (\d+)."
    let moveTapeRegex = "    - Move one slot to the (left|right)."
    let nextStateRegex = "    - Continue with state ([A-Z])."

    let fullRegex = String.concat "/" [
        headerRegex
        currentValueRegex
        newValueRegex
        moveTapeRegex
        nextStateRegex
        currentValueRegex
        newValueRegex
        moveTapeRegex
        nextStateRegex
    ]

    match stateText with
    | Regex fullRegex [ currentState
                        currentValue1
                        newValue1
                        tapeDirection1
                        nextState1
                        currentValue2
                        newValue2
                        tapeDirection2
                        nextState2 ] ->
            Some ([
               ((currentState, int currentValue1),
                (int newValue1, (if tapeDirection1 = "left" then Left else Right), nextState1))
               ((currentState, int currentValue2),
                (int newValue2, (if tapeDirection2 = "left" then Left else Right), nextState2))])

    | _ ->
        printfn "Invalid state text %A" stateText
        None

let instructions =
    statesText
    |> Seq.choose parseState
    |> Seq.collect id
    |> Map.ofSeq

(*printfn "%A" instructions*)

let makeTuringMachine instructions =
    let lookup currentState currentValue =
        Map.find (currentState, currentValue) instructions

    let getValue tapePos tape =
        Map.tryFind tapePos tape |> Option.defaultValue 0

    let step (currentState, tapePos, tape) =
        let currentValue = getValue tapePos tape
        let (newValue, direction, nextState) = lookup currentState currentValue
        
        let newPos = if direction = Left then tapePos - 1 else tapePos + 1

        (nextState, newPos, (Map.add tapePos newValue tape))

    fun state -> let result = step state in Some (result, result)

let machine = makeTuringMachine instructions

let result =
    Seq.unfold machine (initialState, 0, Map.empty)
    |> Seq.skip (diagnosticSteps - 1)
    |> Seq.head
    |> fun (_, _, tape) -> tape
    |> Map.toSeq
    |> Seq.sumBy snd

printfn "%A" result
