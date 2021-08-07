// vim: set et ts=4 sw=4 list :

open System
open System.Text.RegularExpressions

// borrowed from https://theburningmonk.com/2016/12/advent-of-code-fsharp-day-10/
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let readLines filePath = System.IO.File.ReadLines filePath
let input = readLines "input.txt" |> Seq.toList
let inputGap = input |> List.findIndex ((=) "")
let immuneText, infectionText =
    input
    |> List.filter ((<>) "")
    |> List.splitAt inputGap
    // This assumes ordering but oh well
    |> fun (immuneText', infectionText') -> (List.tail immuneText', List.tail infectionText')

let groupRegex = "(\d+) units each with (\d+) hit points (.*) ?with an attack that does (\d+) (\w+) damage at initiative (\d+)"

type UnitGroup = {
    Team: string
    UnitCount: int
    UnitHp: int
    ImmuneTo: Set<string>
    WeakTo: Set<string>
    AttackDamage: int
    AttackType: string
    Initiative: int
}

let makeUnitGroup team unitCount unitHp (weaknessesText: string) attackDamage attackType initiative =
    let weaknesses =
        weaknessesText.Trim([| ')'; '('; ' ' |]).Split("; ")
        |> Array.tryFind (fun s -> s.StartsWith("weak to "))
        |> (function
            | Some ws -> ws.Substring(8).Split(", ")
            | None -> Array.empty)
        |> Set.ofSeq

    let immunities =
        weaknessesText.Trim([| ')'; '('; ' ' |]).Split("; ")
        |> Array.tryFind (fun s -> s.StartsWith("immune to "))
        |> (function
            | Some ws -> ws.Substring(10).Split(", ")
            | None -> Array.empty)
        |> Set.ofSeq

    {
        Team = team
        UnitCount = int unitCount
        UnitHp = int unitHp
        ImmuneTo = immunities
        WeakTo = weaknesses
        AttackDamage = int attackDamage
        AttackType = attackType
        Initiative = int initiative
    }

let parseArmy team inputText =
    inputText
    |> Seq.choose (function
        | Regex groupRegex [unitCount; unitHp; weaknesses; attackDamage; attackType; initiative] ->
            Some (makeUnitGroup team unitCount unitHp weaknesses attackDamage attackType initiative)
        | s ->
            printfn "Bad match: [%A]" s
            None)

let immuneArmy = parseArmy "immune" immuneText
let infectionArmy = parseArmy "infection" infectionText
let allUnits =
    Seq.concat [immuneArmy; infectionArmy]
    |> Seq.map (fun u -> u.Initiative, u)
    |> Map.ofSeq


let effectivePower group = group.UnitCount * group.AttackDamage

let damagePotential attacker target =
    let damageMultiplier =
        if Set.contains attacker.AttackType target.WeakTo then 2
        elif Set.contains attacker.AttackType target.ImmuneTo then 0
        else 1

    (effectivePower attacker) * damageMultiplier

let findTargets units =
    let findTarget (targetMap, unmatched) u =
        let target =
            unmatched
            |> Seq.filter (fun u' -> u'.Team <> u.Team)
            |> Seq.sortByDescending (fun u' -> (damagePotential u u', effectivePower u', u'.Initiative))
            |> Seq.tryHead

        match target with
        | Some target' ->
            (Map.add u.Initiative target'.Initiative targetMap, Set.remove target' unmatched)
        | None -> (targetMap, unmatched)

    let unmatched =
        units
        |> Map.toSeq
        |> Seq.map snd
        |> Set.ofSeq

    units
    |> Map.toList
    |> List.sortByDescending (fun (i, u) -> (effectivePower u, i))
    |> List.map snd
    |> List.fold findTarget (Map.empty, unmatched)
    |> fst
    |> Map.toList
    |> List.sortByDescending id

let doAttackPhase units attackPairs =
    let doAttack units' (attackerId, defenderId) =
        match Map.tryFind attackerId units' with
        | None ->
            units'
        | Some attacker ->
            match Map.tryFind defenderId units' with
            | None ->
                units'
            | Some defender ->
                let attackDamage = damagePotential attacker defender
                let unitsKilled = attackDamage / defender.UnitHp

                if unitsKilled > defender.UnitCount then
                    Map.remove defenderId units'
                else
                    let defender' =
                        { defender with 
                            UnitCount = defender.UnitCount - unitsKilled }

                    Map.add defenderId defender' units'

    attackPairs
    |> List.fold doAttack units

let survivingTeams units =
    units
    |> Map.toSeq
    |> Seq.map (fun (_, u) -> u.Team)
    |> Set.ofSeq

let doBattle units =
    let rec step units' =
        let targets = findTargets units'
        let units'' = doAttackPhase units' targets
        let teams = survivingTeams units''

        if Set.count teams = 1 then
            units''
        else
            step units''

    step units

let countUnits units =
    units
    |> Map.toSeq
    |> Seq.map (fun (_, u) -> u.UnitCount)
    |> Seq.sum

let survivingUnits = doBattle allUnits
printfn "%A" (countUnits survivingUnits)


