#r "nuget: Flips"
#r "/home/ikarni/.nuget/packages/flips/2.4.9/lib/netstandard2.0/Flips.dll"
#I "/home/ikarni/.nuget/packages/google.ortools.runtime.linux-x64/9.0.9048/runtimes/linux-x64/native"
open Flips
open Flips.Types
open Flips.SliceMap

let lines = System.IO.File.ReadAllLines("day19.test")
let splittedLines = lines |> Array.chunkBySize 6

open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

module Map =
    let updateWithDef key def f (m: Map<_, _>) =
        m
        |> Map.tryFind key
        |> Option.defaultValue def
        |> f
        |> fun x -> Map.add key x m

let memoize (f: 'a -> 'b) =
    let cache = System.Collections.Concurrent.ConcurrentDictionary<_, _>(HashIdentity.Structural)
    fun x ->
        cache.GetOrAdd(x, lazy (f x)).Force()


type Resource = {
    Ore: int
    Clay: int
    Obsidian: int
    Geode: int
}

module Resource =
    let empty = { Ore = 0
                  Clay = 0
                  Obsidian = 0
                  Geode = 0 }
    let asList r = [ r.Ore; r.Clay; r.Obsidian; r.Geode ]
    let plus r1 r2 = { Ore = r1.Ore + r2.Ore; Clay = r1.Clay + r2.Clay; Obsidian = r1.Obsidian + r2.Obsidian; Geode = r1.Geode + r2.Geode }
    let minus r1 r2 = { Ore = r1.Ore - r2.Ore; Clay = r1.Clay - r2.Clay; Obsidian = r1.Obsidian - r2.Obsidian; Geode = r1.Geode - r2.Geode }
    let mult x r1 = { Ore = r1.Ore * x; Clay = r1.Clay * x; Obsidian = r1.Obsidian * x; Geode = r1.Geode * x }

type Robot =
    | OreRobot
    | ClayRobot
    | ObsidianRobot
    | GeodeRobot

module Robot =
    let collect = function
        | OreRobot -> { Resource.empty with Ore = 1 }
        | ClayRobot -> { Resource.empty with Clay = 1 }
        | ObsidianRobot -> { Resource.empty with Obsidian = 1 }
        | GeodeRobot -> { Resource.empty with Geode = 1 }

type Blueprint = (Robot * Resource) list

type State = {
    Resource: Resource
    Robots: Map<Robot, int>
    Steps: int
}

let neighF blueprint (s : State) =
    //printfn $"{s}"
    if s.Steps <= 0 then [] else
    let collectF s = { s with Resource = s.Robots |> Map.toSeq |> Seq.map (fun (r, x) -> Resource.mult x (Robot.collect r)) |> Seq.fold Resource.plus s.Resource }
    let moves = 
        blueprint |> List.rev |> List.map (fun (r, res) -> { Resource = Resource.minus s.Resource res; Robots = Map.updateWithDef r 0 ((+) 1) s.Robots; Steps = s.Steps - 1 })
        |> List.filter (fun s -> s.Resource |> Resource.asList |> List.forall (fun x -> x >= 0))
    moves @ [{s with Steps = s.Steps - 1}]
    |> List.map collectF

let rec opt = memoize <| fun (blueprint, s) ->
    match neighF blueprint s |> List.map (fun x -> opt (blueprint, x)) with
    | [] -> s.Resource.Geode, s
    | xs -> xs |> Seq.maxBy fst

let rec greedyOpt = memoize <| fun (blueprint, s) ->
    match neighF blueprint s |> List.tryHead |> Option.map (fun x -> greedyOpt (blueprint, x)) with
    | None -> s.Resource.Geode, s
    | Some x -> x


let readBlueprint (arr: string[]) = [
        match arr[1] with
        | Match "Each ore robot costs (.*) ore." [ s ] -> OreRobot, { Resource.empty with Ore = int s }
    
        match arr[2] with
        | Match "Each clay robot costs (.*) ore." [ s ] -> ClayRobot, { Resource.empty with Ore = int s }
    
        match arr[3] with
        | Match "Each obsidian robot costs (.*) ore and (.*) clay." [ s1; s2 ] -> ObsidianRobot, { Resource.empty with Ore = int s1; Clay = int s2 }
    
        match arr[4] with
        | Match "Each geode robot costs (.*) ore and (.*) obsidian." [ s1; s2 ] -> GeodeRobot, { Resource.empty with Ore = int s1; Obsidian = int s2 }
    ]

let blueprints = splittedLines |> Array.map readBlueprint

let init = { Resource = Resource.empty; Robots = Map.ofList [ OreRobot, 1 ]; Steps = 20 }

let model (blueprint : Blueprint) totalSteps =
    let steps = [1..totalSteps]
    let robotTypes = [ OreRobot; ClayRobot; ObsidianRobot; GeodeRobot ]
    let costs = blueprint |> List.collect (fun (r, res) -> [(r, OreRobot), float res.Ore; (r, ClayRobot), float res.Clay; (r, ObsidianRobot), float res.Obsidian; (r, GeodeRobot), float res.Geode]) |> SMap2.ofList
    let i = Integer(0.0,1.0)
    let robots = 
        DecisionBuilder "robot" {
            for _ in steps do
                for t in robotTypes ->
                    Integer (0.0, (float totalSteps))
            }
            |> SMap2.ofSeq
    let robotsCons = 
        ConstraintBuilder "robotsNotDecrease" {
            for (i,j) in Seq.pairwise steps do
                for t in robotTypes ->
                    robots[i, t] <== robots[j, t]
            }
    let buys = 
        ConstraintBuilder "buy" {
            for i in steps do
                for t in robotTypes ->
                    let x = robots[i,All] .* costs[All, t]
                    sum robots[LessThan i, t] + (if t=OreRobot then float i else 0.0) >== sum (robots[i,All] .* costs[All,t])
            }   
    let objExpr = sum robots.[All, GeodeRobot]
    let objective = Objective.create "Obj" Maximize objExpr
    let model =
        Model.create objective
        |> Model.addConstraints robotsCons
        |> Model.addConstraints buys
    match Solver.solve { Settings.basic with MaxDuration = 60_000; WriteLPFile = Some "day19.lp" } model with
    | Optimal sol ->
        sol.DecisionResults |> Map.filter (fun _ x -> x > 0.0) |> Map.toSeq |> Seq.map (fun (k, x) -> k.Name, x) |> Seq.sort |> Seq.iter (printfn "%A")
        sol.ObjectiveResult |> int


let part1 = model blueprints[0] 24

printfn $"PART1: {part1}"

let part2 = 0

printfn $"PART2: {part2}"
