let lines = System.IO.File.ReadAllLines("day14.test")

let wallPaths =
    lines
    |> Array.map (fun s ->
        s.Split " -> "
        |> Seq.map (fun x -> x.Split "," |> fun xs -> int xs[0], int xs[1])
        |> Seq.toList)

let fromTo a b =
    if a = b then [ a ] else [ a .. (sign (b - a)) .. b ]

let walls =
    wallPaths
    |> Seq.collect (fun path ->
        path
        |> List.pairwise
        |> List.collect (fun ((x1, y1), (x2, y2)) ->
            fromTo x1 x2 |> List.collect (fun x -> fromTo y1 y2 |> List.map (fun y -> x, y))))
    |> Seq.distinct
    |> Seq.toList

let posPlus (a, b) (c, d) = a + c, b + d
let posMinus (a, b) (c, d) = a - c, b - d

let fallDirs = [ 0, 1; 1, 1; -1, 1 ]

type Block =
    | Wall
    | Sand

let initCave = (Map.empty, walls) ||> Seq.fold (fun m w -> m |> Map.add w Wall)
let spawnSand = 500, 0

let maxWallY = walls |> Seq.map snd |> Seq.max

let simStep (sand, cave) =
    match
        fallDirs
        |> Seq.map (posPlus sand)
        |> Seq.tryFind (fun pos -> Map.containsKey pos cave |> not)
    with
    | None -> Some(spawnSand, cave |> Map.add sand Sand)
    | Some((_, y) as pos) -> if y >= maxWallY then None else Some(pos, cave)

let caveSim =
    (spawnSand, initCave)
    |> Seq.unfold (fun s -> simStep s |> Option.map (fun x -> x, x))

let finalCave = Seq.last caveSim |> snd

let part1 = finalCave |> Map.filter (fun _ b -> b = Sand) |> Map.count
printfn $"PART1: {part1}"

let part2 = 0
printfn $"PART2: {part2}"
