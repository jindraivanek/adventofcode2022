let lines = System.IO.File.ReadAllLines("day12.test")
let n = Array.length lines
printfn "%A" lines
let grid = Array2D.init n n (fun i j -> lines[i][j])

module Array2D =
    let indexedValues arr =
        seq { 0 .. Array2D.length1 arr - 1 }
        |> Seq.collect (fun i -> seq { 0 .. Array2D.length2 arr - 1 } |> Seq.map (fun j -> (i, j), arr[i, j]))

    let findIndex condition arr =
        indexedValues arr |> Seq.find (snd >> condition) |> fst

let elevation (x, y) grid =
    match Array2D.get grid x y with
    | 'S' -> 'a'
    | 'E' -> 'z'
    | x -> x
    |> int

let directions (x, y) grid =
    [ x - 1, y; x + 1, y; x, y - 1; x, y + 1 ]
    |> List.filter (fun (i, j) -> i >= 0 && j >= 0 && i < Array2D.length1 grid && j < Array2D.length2 grid)

let neigh grid x =
    directions x grid
    |> List.filter (fun y -> elevation y grid - 1 <= elevation x grid)

let startNode grid =
    Array2D.findIndex
        (function
        | 'S' -> true
        | _ -> false)
        grid

let isEndNode grid (x, y) =
    match Array2D.get grid x y with
    | 'E' -> true
    | _ -> false

let dijsktra (initNode: 'n) (neighF: 'n -> 'n list) (finishCond: 'n -> bool) =
    let pq = System.Collections.Generic.PriorityQueue<'n, int>()
    let visited = System.Collections.Generic.HashSet<'n>()

    let dequeue () =
        let (success, node, p) = pq.TryDequeue()
        printfn "%A" (node, p, elevation node grid)
        if success then Some(node, p) else None

    pq.Enqueue(initNode, 0)

    let rec step () =
        match dequeue () with
        | Some(node, p) when visited.Contains node -> step ()
        | Some(node, p) when finishCond node -> Some(node, p)
        | None -> None
        | Some(node, p) ->
            visited.Add node
            neighF node |> Seq.iter (fun n -> pq.Enqueue(n, p + 1))
            step ()

    step ()

let part1 = dijsktra (startNode grid) (neigh grid) (isEndNode grid)

printfn $"PART1: {part1}"

let part2 = 0

printfn $"PART2: {part2}"
