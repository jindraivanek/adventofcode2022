#time
let lines = System.IO.File.ReadAllLines("day16.input")

open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

type Label = Label of string

let valves =
    lines
    |> Array.map (function
        | Match "Valve (.*) has flow rate=(.*)\; tunnel[s]? lead[s]? to valve[s]? (.*)" [ v; flow; edges ] ->
            let tunnels = edges.Split ", " |> Seq.map Label |> Seq.toList
            Label v, {| Flow = int flow; Tunnels = tunnels |}
        | s -> failwith $"no match: {s}")
    |> Map.ofArray

module Map =
    let addMin key value m =
        m
        |> Map.change key (function
            | None -> Some value
            | Some x -> Some(min x value))

let shortestPaths (nodes: 'n seq) (neighF: 'n -> 'n seq) =
    let edges = nodes |> Seq.collect (fun v -> neighF v |> Seq.map (fun w -> v, w))
    let m = (Map.empty, edges) ||> Seq.fold (fun m (v, w) -> Map.add (v, w) 1 m)
    let m = (m, nodes) ||> Seq.fold (fun m v -> Map.add (v, v) 0 m)

    (m, nodes)
    ||> Seq.fold (fun m k ->
        (m, nodes)
        ||> Seq.fold (fun m i ->
            (m, nodes)
            ||> Seq.fold (fun m j ->
                Option.map2 (+) (Map.tryFind (i, k) m) (Map.tryFind (k, j) m)
                |> Option.map (fun c -> Map.addMin (i, j) c m)
                |> Option.defaultValue m)))

let dijsktra (initNodes: ('n * int) list) (neighF: 'n -> ('n * int) list) (finishCond: 'n -> bool) =
    let pq = System.Collections.Generic.PriorityQueue<'n, int>()
    let visited = System.Collections.Generic.HashSet<'n>()

    let dequeue () =
        let (success, node, p) = pq.TryDequeue()

        if success then
            //if visited.Count % 1000 = 0 then printfn "%A" (node, p)
            Some(node, p)
        else
            None

    initNodes |> Seq.iter (fun (n, p) -> pq.Enqueue(n, p))

    let rec step () =
        match dequeue () with
        | Some(node, p) when visited.Contains node -> step ()
        | Some(node, p) when finishCond node -> Some(node, p)
        | None -> None
        | Some(node, p) ->
            visited.Add node
            neighF node |> Seq.iter (fun (n, p') -> pq.Enqueue(n, p + p'))
            step ()

    step ()

let paths = shortestPaths (valves |> Map.keys) (fun n -> valves[n].Tunnels)

let pathsFlow =
    paths
    |> Map.map (fun (_, target) v -> v + 1, valves[target].Flow)
    |> Map.filter (fun _ (_, f) -> f > 0)
    |> Map.toSeq
    |> Seq.groupBy (fst >> fst)
    |> Seq.map (fun (k, g) -> k, g |> Seq.map (fun ((_, t), (c, f)) -> t, c, f) |> Seq.toList)
    |> Map.ofSeq

let totalSteps = 30
let maxFlow = valves |> Map.values |> Seq.map (fun v -> v.Flow) |> Seq.max //|> fun x -> x * valves.Count

type Agent = { Steps: int; Pos: Label }

type Node =
    { Agents: Map<int, Agent>
      Score: int
      OpenedValves: Label Set }

let neighF n =
    // n.Agents
    // |> Map.toSeq
    // |> Seq.collect (fun (agentId, a) ->
        let (agentId, a) = n.Agents |> Map.toSeq |> Seq.maxBy (fun (_, a) -> a.Steps)
        let pos = a.Pos
        let steps = a.Steps

        let moves =
            pathsFlow
            |> Map.tryFind pos
            |> Option.defaultValue []
            |> List.filter (fun (l, c, f) -> c <= steps && not (Set.contains l n.OpenedValves))
            |> List.map (fun (l, c, f) ->
                let score = (steps - c) * f
                let p = c * maxFlow * totalSteps - score

                { Agents = Map.add agentId { Steps = steps - c; Pos = l } n.Agents
                  Score = n.Score + score
                  OpenedValves = Set.add l n.OpenedValves },
                p)

        let endMove =
            { n with Agents = Map.add agentId { a with Steps = 0 } n.Agents }, steps * maxFlow * totalSteps

        endMove :: moves
        //)
        |> Seq.toList
        //|> List.maxBy (fun (n, _) -> n.Score) |> List.singleton

let part1 =
    let initState =
        { Agents = Map.ofSeq [ 1, { Steps = totalSteps; Pos = Label "AA" } ]
          Score = 0
          OpenedValves = Set.empty }

    dijsktra [ initState, 0 ] neighF (fun n -> n.Agents |> Map.forall (fun _ a -> a.Steps = 0))

printfn $"PART1: {part1}"

let part2 =
    let totalSteps = 26

    let initState =
        { Agents =
            Map.ofSeq
                [ 1, { Steps = totalSteps; Pos = Label "AA" }
                  2, { Steps = totalSteps; Pos = Label "AA" } ]
          Score = 0
          OpenedValves = Set.empty }

    dijsktra [ initState, 0 ] neighF (fun n -> n.Agents |> Map.forall (fun _ a -> a.Steps = 0))

printfn $"PART2: {part2}"
