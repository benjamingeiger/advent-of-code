// vim: set et ts=4 sw=4 list :

open System

#load "../utils.fsx"
open Utils

let rawInput = readLines "input.txt" |> List.ofSeq

let parseInput rawInput =
    rawInput
    |> List.choose (function
        | Regex
            "Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian."
            [blueprintId; oreOreCost; clayOreCost; obsidianOreCost; obsidianClayCost; geodeOreCost; geodeObsidianCost] ->
                Some (int blueprintId, int oreOreCost, int clayOreCost, int obsidianOreCost, int obsidianClayCost, int geodeOreCost, int geodeObsidianCost)
        | s ->
            eprintfn "Misparsed line: %s" s
            None)

let evaluateBlueprint (blueprintId, oreOreCost, clayOreCost, obsidianOreCost, obsidianClayCost, geodeOreCost, geodeObsidianCost) =

    let maxOreCost = List.max [oreOreCost; clayOreCost; obsidianOreCost; geodeOreCost]

    printfn "evaluating %A" blueprintId

    let possibleSteps = memoize (fun ((oreCount, clayCount, obsidianCount, geodeCount), (oreRobots, clayRobots, obsidianRobots, geodeRobots)) ->
        let buildNothing () = 
            if oreCount <= (oreOreCost + clayOreCost + obsidianOreCost + geodeOreCost) then
                [((oreCount + oreRobots, clayCount + clayRobots, obsidianCount + obsidianRobots, geodeCount + geodeRobots), (oreRobots, clayRobots, obsidianRobots, geodeRobots))]
            else []

        let buildOreRobot () =
            if oreCount >= oreOreCost && (oreRobots < maxOreCost) then
                [((oreCount + oreRobots - oreOreCost, clayCount + clayRobots, obsidianCount + obsidianRobots, geodeCount + geodeRobots), (oreRobots + 1, clayRobots, obsidianRobots, geodeRobots))]
            else []

        let buildClayRobot () =
            if oreCount >= clayOreCost && clayRobots < obsidianClayCost then
                [((oreCount + oreRobots - clayOreCost, clayCount + clayRobots, obsidianCount + obsidianRobots, geodeCount + geodeRobots), (oreRobots, clayRobots + 1, obsidianRobots, geodeRobots))]
            else []

        let buildObsidianRobot () =
            if oreCount >= obsidianOreCost && clayCount >= obsidianClayCost && obsidianRobots < geodeObsidianCost then
                [((oreCount + oreRobots - obsidianOreCost, clayCount + clayRobots - obsidianClayCost, obsidianCount + obsidianRobots, geodeCount + geodeRobots), (oreRobots, clayRobots, obsidianRobots + 1, geodeRobots))]
            else []

        let buildGeodeRobot () =
            if oreCount >= geodeOreCost && obsidianCount >= geodeObsidianCost then
                [((oreCount + oreRobots - geodeOreCost, clayCount + clayRobots, obsidianCount + obsidianRobots - geodeObsidianCost, geodeCount + geodeRobots), (oreRobots, clayRobots, obsidianRobots, geodeRobots + 1))]
            else []

        let geodeRobotStep = buildGeodeRobot ()
        if List.length geodeRobotStep > 0 then
            geodeRobotStep // |> List.map (fun (counts, robots) -> (n - 1, counts, robots))
        else
            [buildObsidianRobot; buildNothing; buildOreRobot; buildClayRobot]
            |> List.collect (fun f -> f ())
            |> List.map (fun ((oreCount, clayCount, obsidianCount, geodeCount), robots) ->
                (
                    min oreCount (2 * maxOreCost),
                    min clayCount (2 * obsidianClayCost),
                    min obsidianCount (2 * geodeObsidianCost),
                    geodeCount
                ), robots))
            // |> List.map (fun (counts, robots) -> (n - 1, counts, robots)))

    let rec step (maxGeodeBots, visitedStates) = function
        | [] -> visitedStates
        | (n, counts, robots) :: rest ->
            let (_, _, _, geodeBots) = robots
            if (Set.contains (counts, robots) visitedStates) then
                step (maxGeodeBots, visitedStates) rest
            elif n <= 0 then
                step ((max geodeBots maxGeodeBots), Set.add (counts, robots) visitedStates) rest
            elif geodeBots < maxGeodeBots - 1 then
                step (maxGeodeBots, visitedStates) rest
            else
                let ns =
                    possibleSteps (counts, robots)
                    |> List.filter (fun (_, (_, _, _, geodeBots')) -> geodeBots' >= maxGeodeBots - 1)
                    |> List.map (fun (counts, robots) -> (n - 1, counts, robots))

                step ((max geodeBots maxGeodeBots), Set.add (counts, robots) visitedStates) (rest @ ns)

    step (0, Set.empty) [(32, (0, 0, 0, 0), (1, 0, 0, 0))]
    |> Set.toList
    |> List.map (fun ((_, _, _, geodes), _) -> geodes)
    |> List.max

let run input =
    input
    |> List.truncate 3
    |> List.map evaluateBlueprint
    |> List.fold ( * ) 1

doProcess parseInput run rawInput
