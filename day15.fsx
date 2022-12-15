#time
let lines = System.IO.File.ReadAllLines("day15.input")

open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

let sensorsBeacons =
    lines
    |> Array.map (function
        | Match "Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)" [ x1; y1; x2; y2 ] ->
            (int x1, int y1), (int x2, int y2))

let dist (a, b) (c, d) = abs (a - c) + abs (b - d)

let fromTo a b =
    if a = b then
        Seq.singleton a
    else
        seq { a .. (sign (b - a)) .. b }

let fromToPos a b =
    if a = b then
        Seq.singleton a
    else
        Seq.zip (fromTo (fst a) (fst b)) (fromTo (snd a) (snd b))

let sensorsRadius = sensorsBeacons |> Array.map (fun (s, b) -> s, dist s b)
let beacons = sensorsBeacons |> Seq.map snd |> set

let inspectY yLine searchArea inverse =
    let nearSensors =
        sensorsRadius |> Array.filter (fun ((_, y), r) -> abs (y - yLine) <= r)

    let beacons = beacons |> Seq.filter (fun (_, y) -> y = yLine)

    let (minX, maxX) =
        searchArea
        |> Option.defaultWith (fun () ->
            let minX =
                nearSensors |> Seq.minBy (fun ((x, _), _) -> x) |> (fun ((x, _), r) -> x - r)

            let maxX =
                nearSensors |> Seq.maxBy (fun ((x, _), _) -> x) |> (fun ((x, _), r) -> x + r)

            minX, maxX)

    let condition pos =
        nearSensors |> Seq.exists (fun (s, r) -> dist pos s <= r)

    let beaconcondition pos = not (beacons |> Seq.exists ((=) pos))

    [ minX..maxX ]
    |> List.map (fun x -> x, yLine)
    |> List.filter (
        if inverse then
            condition >> not
        else
            fun x -> condition x && beaconcondition x
    )

let sensorRadiusEdge maxCoord ((x, y), r) =
    [ x - r - 1, y; x, y - r - 1; x + r + 1, y; x, y + r + 1; x - r - 1, y ]
    |> List.pairwise
    |> Seq.collect (fun (a, b) -> fromToPos a b)
    |> Seq.filter (fun ((x, y) as pos) -> x >= 0 && y >= 0 && x <= maxCoord && y <= maxCoord)

let notCoveredEdges maxCoord =
    sensorsRadius
    |> Seq.collect (sensorRadiusEdge maxCoord)
    |> Seq.filter (fun ((x, y) as pos) ->
        not (Set.contains pos beacons)
        && sensorsRadius |> Seq.forall (fun (s, r) -> dist pos s > r))
    |> Seq.distinct

let part1 = inspectY 2000000 None false |> List.length

printfn $"PART1: {part1}"

let part2 =
    notCoveredEdges 4000000
    |> Seq.head
    |> fun (x, y) -> int64 x * 4000000L + int64 y

printfn $"PART2: {part2}"
