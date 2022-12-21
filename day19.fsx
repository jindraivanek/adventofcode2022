#r "nuget: Flips"
#r "/home/ikarni/.nuget/packages/flips/2.4.9/lib/netstandard2.0/Flips.dll"
#I "/home/ikarni/.nuget/packages/google.ortools.runtime.linux-x64/9.0.9048/runtimes/linux-x64/native"

open Flips
open Flips.Types
open Flips.SliceMap

let lines = System.IO.File.ReadAllLines("day19.input")

open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

type Resource =
    | Ore
    | Clay
    | Obsidian
    | Geode

type Robot =
    | OreRobot
    | ClayRobot
    | ObsidianRobot
    | GeodeRobot

let readBlueprint (s: string) =
    match s with
    | Match "Each ore robot costs (.*) ore. Each clay robot costs (.*) ore. Each obsidian robot costs (.*) ore and (.*) clay. Each geode robot costs (.*) ore and (.*) obsidian." 
      [ oo
        co
        bo
        bc
        go
        gb ] ->
        [ OreRobot, [ Ore, int oo ]
          ClayRobot, [ Ore, int co ]
          ObsidianRobot, [ Ore, int bo; Clay, int bc ]
          GeodeRobot, [ Ore, int go; Obsidian, int gb ] ]

let blueprints = lines |> Array.map readBlueprint

let model blueprint totalSteps =
    let steps = [ 1..totalSteps ]
    let robotTypes = [ OreRobot; ClayRobot; ObsidianRobot; GeodeRobot ]
    let resourceTypes = [ Ore; Clay; Obsidian; Geode ]

    let costs =
        blueprint
        |> List.collect (fun (r, res) -> res |> List.map (fun (o, x) -> (r, o), float x))
        |> SMap2.ofList

    let robots =
        DecisionBuilder "robot" {
            for _ in steps do
                for t in robotTypes -> Boolean
        }
        |> SMap2.ofSeq

    let robotsCons =
        ConstraintBuilder "factoryLimit" {
            for i in steps do
                yield sum robots[i, All] <== 1.0
        }

    let collectMult i =
        [ 1 .. i - 1 ] |> List.map (fun j -> j, float (i - j)) |> SMap.ofList

    let buys =
        ConstraintBuilder "buy" {
            for i in steps do
                for (t, r) in Seq.zip robotTypes resourceTypes ->
                    sum (robots[LessThan(i - 1), t] .* (collectMult (i - 1)))
                    + (if t = OreRobot && i > 1 then float (i - 1) else 0.0)
                    >== sum (robots[LessOrEqual i, All] .* costs[All, r])
        }

    let objExpr = sum (robots.[All, GeodeRobot] .* collectMult totalSteps)
    let objective = Objective.create "Obj" Maximize objExpr

    let model =
        Model.create objective
        |> Model.addConstraints robotsCons
        |> Model.addConstraints buys

    match Solver.solve Settings.basic model with
    | Optimal sol ->
        //sol.DecisionResults |> Map.filter (fun _ x -> x > 0.0) |> Map.toSeq |> Seq.map (fun (k, x) -> k.Name, x) |> Seq.sort |> Seq.iter (printfn "%A")
        sol.ObjectiveResult |> int


let part1 () =
    let results = blueprints |> Array.mapi (fun i b -> i, model b 24)
    printfn "%A" results
    results |> Seq.map (fun (i, x) -> (i + 1) * x) |> Seq.sum

printfn $"PART1: {part1 ()}"

let part2 () =
    let results = blueprints[..2] |> Array.map (fun b -> model b 32)
    printfn "%A" results
    results |> Seq.reduce (*)

printfn $"PART2: {part2 ()}"
