#time
let lines = System.IO.File.ReadAllLines("day16.test")

open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

type Label = Label of string

let valves =
    lines |> Array.map (function
        | Match "Valve (.*) has flow rate=(.*)\; tunnel[s]? lead[s]? to valve[s]? (.*)" [v; flow; edges] -> 
            let tunnels = edges.Split ", " |> Seq.map Label |> Seq.toList
            Label v, {| Flow = int flow; Tunnels = tunnels |}
        | s -> failwith $"no match: {s}")
    |> Map.ofArray

let dijsktra (initNodes: ('n * int) list) (neighF: 'n -> ('n * int) list) (finishCond: 'n -> bool) =
    let pq = System.Collections.Generic.PriorityQueue<'n, int>()
    let visited = System.Collections.Generic.HashSet<'n>()

    let dequeue () =
        let (success, node, p) = pq.TryDequeue()

        if success then
            printfn "%A" (node, p)
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

let totalSteps = 30
let maxFlow = valves |> Map.values |> Seq.map (fun v -> v.Flow) |> Seq.max |> fun x -> x * valves.Count

type Node = { Steps : int; Score : int; Pos : Label; OpenedValves : Label list }
let initState = { Steps = totalSteps; Score = 0; Pos = Label "AA"; OpenedValves = [] }
let neighF n = 
    let score = n.OpenedValves |> Seq.sumBy (fun l -> valves[l].Flow)
    let p = maxFlow - score
    let moves = valves[n.Pos].Tunnels |> List.map (fun l -> { Steps = n.Steps - 1; Score = n.Score + score; Pos = l; OpenedValves = n.OpenedValves }, p)
    let openValve = { Steps = n.Steps - 1; Score = n.Score + score; Pos = n.Pos; OpenedValves = n.Pos :: n.OpenedValves }, p
    openValve :: moves

let res = dijsktra [initState, 0] neighF (fun n -> n.Steps = 0)


let part1 = 0

printfn $"PART1: {part1}"

let part2 =
    0

printfn $"PART2: {part2}"
