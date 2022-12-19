module Day19

open System
open System.IO

type Blueprint =
    {
        Id: int
        OreRobotCostInOre: int
        ClayRobotCostInOre: int
        ObsidianRobotCostInOre: int
        ObsidianRobotCostInClay: int
        GeodeRobotCostInOre: int
        GeodeRobotCostInObsidian: int
        MaxOreSpend: int
    }

let toBlueprint (s: string) =
    let colon = s.IndexOf(':')
    let blueprintId = s.Substring(10, colon - 10) |> int
    
    let oreRobotCostOre = s.Split("Each ore robot costs ").[1].Substring(0, 1) |> int
    let clayRobotCostOre = s.Split("Each clay robot costs ").[1].Substring(0, 1) |> int
    let obsidianRobotCostOre = s.Split("Each obsidian robot costs ").[1].Substring(0, 1) |> int
    let obsidianRobotCostClay = s.Split("and ").[1].Substring(0, 2).Trim() |> int
    let geodeRobotCostOre = s.Split("Each geode robot costs ").[1].Substring(0, 1) |> int
    let geodeRobotCostObsidian = s.Split("Each geode robot costs ").[1].Split(" and ").[1].Substring(0, 2).Trim() |> int

    {
        Id = blueprintId
        OreRobotCostInOre = oreRobotCostOre
        ClayRobotCostInOre = clayRobotCostOre
        ObsidianRobotCostInOre = obsidianRobotCostOre
        ObsidianRobotCostInClay = obsidianRobotCostClay
        GeodeRobotCostInOre = geodeRobotCostOre
        GeodeRobotCostInObsidian = geodeRobotCostObsidian
        MaxOreSpend = Array.max [| oreRobotCostOre; clayRobotCostOre; obsidianRobotCostOre; geodeRobotCostOre |]
    }

let allBlueprints =
    File.ReadAllLines(@"input\day19.txt")
    |> Array.map toBlueprint

type State =
    {
        TimeRemaining: int
        Ore: int
        Clay: int
        Obsidian: int
        Geodes: int
        OreRobots: int
        ClayRobots: int
        ObsidianRobots: int
        GeodeRobots: int
    }

let timePasses state =
    { state with
        TimeRemaining = state.TimeRemaining - 1
        Ore = state.Ore + state.OreRobots
        Clay = state.Clay + state.ClayRobots
        Obsidian = state.Obsidian + state.ObsidianRobots
        Geodes = state.Geodes + state.GeodeRobots
    }

let canBuildOreRobot blueprint state = state.Ore >= blueprint.OreRobotCostInOre
let canBuildClayRobot blueprint state = state.Ore >= blueprint.ClayRobotCostInOre
let canBuildObsidianRobot blueprint state = state.Ore >= blueprint.ObsidianRobotCostInOre && state.Clay >= blueprint.ObsidianRobotCostInClay
let canBuildGeodeRobot blueprint state = state.Ore >= blueprint.GeodeRobotCostInOre && state.Obsidian >= blueprint.GeodeRobotCostInObsidian

let buildOreRobot blueprint state =
    { state with
        TimeRemaining = state.TimeRemaining - 1
        Ore = state.Ore + state.OreRobots - blueprint.OreRobotCostInOre
        Clay = state.Clay + state.ClayRobots
        Obsidian = state.Obsidian + state.ObsidianRobots
        Geodes = state.Geodes + state.GeodeRobots
        OreRobots = state.OreRobots + 1
    }

let buildClayRobot blueprint state =
    { state with
        TimeRemaining = state.TimeRemaining - 1
        Ore = state.Ore + state.OreRobots - blueprint.ClayRobotCostInOre
        Clay = state.Clay + state.ClayRobots
        Obsidian = state.Obsidian + state.ObsidianRobots
        Geodes = state.Geodes + state.GeodeRobots
        ClayRobots = state.ClayRobots + 1
    }

let buildObsidianRobot blueprint state =
    { state with
        TimeRemaining = state.TimeRemaining - 1
        Ore = state.Ore + state.OreRobots - blueprint.ObsidianRobotCostInOre
        Clay = state.Clay + state.ClayRobots - blueprint.ObsidianRobotCostInClay
        Obsidian = state.Obsidian + state.ObsidianRobots
        Geodes = state.Geodes + state.GeodeRobots
        ObsidianRobots = state.ObsidianRobots + 1
    }

let buildGeodeRobot blueprint state =
    { state with
        TimeRemaining = state.TimeRemaining - 1
        Ore = state.Ore + state.OreRobots - blueprint.GeodeRobotCostInOre
        Clay = state.Clay + state.ClayRobots
        Obsidian = state.Obsidian + state.ObsidianRobots - blueprint.GeodeRobotCostInObsidian
        Geodes = state.Geodes + state.GeodeRobots
        GeodeRobots = state.GeodeRobots + 1
    }

let rec maxGeodes blueprint skippedOre skippedClay bestSoFar state =
    let maxG = maxGeodes blueprint
    let maxGnoSkip = maxGeodes blueprint false false

    if state.TimeRemaining = 0 then
        state.Geodes
    else
        let maxPossible = state.Geodes + state.GeodeRobots * state.TimeRemaining + (state.TimeRemaining - 1) * state.TimeRemaining / 2
        if maxPossible < bestSoFar then
            0
        elif canBuildGeodeRobot blueprint state then
            buildGeodeRobot blueprint state |> maxGnoSkip bestSoFar // always build a geode robot if possible
        else
            let canBuildOre = canBuildOreRobot blueprint state && state.OreRobots < blueprint.MaxOreSpend && not skippedOre
            let canBuildClay = canBuildClayRobot blueprint state && state.ClayRobots < blueprint.ObsidianRobotCostInClay && not skippedClay
            let canBuildObsidian = canBuildObsidianRobot blueprint state

            match canBuildObsidian, canBuildClay, canBuildOre with
            | false, true, true ->
                let c = buildClayRobot blueprint state |> maxGnoSkip bestSoFar
                let best = max c bestSoFar
                let o = buildOreRobot blueprint state |> maxGnoSkip best
                let best2 = max best o
                let s = timePasses state |> maxG true true best2
                max best2 s
            | false, true, false ->
                let c = buildClayRobot blueprint state |> maxGnoSkip bestSoFar
                let best = max c bestSoFar
                let s = timePasses state |> maxG false true best
                max best s
            | false, false, true ->
                let o = buildOreRobot blueprint state |> maxGnoSkip bestSoFar
                let best = max o bestSoFar
                let s = timePasses state |> maxG true false best
                max best s
            | false, false, false ->
                let s = timePasses state |> maxGnoSkip bestSoFar
                s
            | true, true, true ->
                let ob = buildObsidianRobot blueprint state |> maxGnoSkip bestSoFar
                let best = max ob bestSoFar
                let c = buildClayRobot blueprint state |> maxGnoSkip best
                let best2 = max c best
                let o = buildOreRobot blueprint state |> maxGnoSkip best2
                let best3 = max o best2
                let s = timePasses state |> maxG true true best3
                max s best3
            | true, true, false ->
                let ob = buildObsidianRobot blueprint state |> maxGnoSkip bestSoFar
                let best = max ob bestSoFar
                let c = buildClayRobot blueprint state |> maxGnoSkip best
                let best2 = max c best
                let s = timePasses state |> maxG false true best2
                max s best2
            | true, false, true ->
                let ob = buildObsidianRobot blueprint state |> maxGnoSkip bestSoFar
                let best = max ob bestSoFar
                let o = buildOreRobot blueprint state |> maxGnoSkip best
                let best2 = max o best
                let s = timePasses state |> maxG true false best2
                max s best2
            | true, false, false ->
                let ob = buildObsidianRobot blueprint state |> maxGnoSkip bestSoFar
                let best = max ob bestSoFar
                let s = timePasses state |> maxGnoSkip best
                max s best

let getMaxGeodes blueprint initialState = maxGeodes blueprint false false 0 initialState

let part1() =
    let initialState =
        {
            TimeRemaining = 24
            Ore = 0
            Clay = 0
            Obsidian = 0
            Geodes = 0
            OreRobots = 1
            ClayRobots = 0
            ObsidianRobots = 0
            GeodeRobots = 0
        }
 
    let qualityLevels =
        allBlueprints
        |> Array.map (fun blueprint -> blueprint, getMaxGeodes blueprint initialState)
        |> Array.map (fun (blueprint, maxGeodes) -> blueprint.Id * maxGeodes)
    
    let totalQualityLevels = Array.sum qualityLevels

    printfn "Total quality levels: %i" totalQualityLevels

let part2() =
    let initialState =
        {
            TimeRemaining = 32
            Ore = 0
            Clay = 0
            Obsidian = 0
            Geodes = 0
            OreRobots = 1
            ClayRobots = 0
            ObsidianRobots = 0
            GeodeRobots = 0
        }

    let geodes1 = getMaxGeodes allBlueprints.[0] initialState
    let geodes2 = getMaxGeodes allBlueprints.[1] initialState
    let geodes3 = getMaxGeodes allBlueprints.[2] initialState

    let product = geodes1 * geodes2 * geodes3

    printfn "Product: %i" product
