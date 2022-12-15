#time
let lines = System.IO.File.ReadAllLines("day15.test")

open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

let sensorsBeacons =
    lines |> Array.map (function
        | Match "Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)" [x1; y1; x2; y2] -> (int x1, int y1), (int x2, int y2))

let posPlus (a, b) (c, d) = a + c, b + d
let posMinus (a, b) (c, d) = a - c, b - d
let dist (a, b) (c, d) = abs (a - c) + abs (b - d)


let inspectY sensorsBeacons yLine searchArea inverse =
    let sensorsRadius = sensorsBeacons |> Array.map (fun (s, b) -> s, dist s b)
    let nearSensors = sensorsRadius |> Array.filter (fun ((_, y), r) -> abs (y - yLine) <= r)
    let beacons = sensorsBeacons |> Seq.map snd |> Seq.filter (fun (_, y) -> y = yLine) |> Seq.toArray
    let (minX, maxX) = searchArea |> Option.defaultWith (fun () ->
        let minX = nearSensors |> Seq.minBy (fun ((x, _), _) -> x) |> fun ((x, _), r) -> x - r
        let maxX = nearSensors |> Seq.maxBy (fun ((x, _), _) -> x) |> fun ((x, _), r) -> x + r
        minX, maxX)
    let condition pos = nearSensors |> Seq.exists (fun (s, r) -> dist pos s <= r)
    let beaconcondition pos = not (beacons |> Seq.exists ((=) pos))
    [ minX .. maxX ] |> List.map (fun x -> x, yLine) |> List.filter (if inverse then condition >> not else fun x -> condition x && beaconcondition x)

let part1Test =
    inspectY sensorsBeacons 10 None false |> List.length

//let part1 = inspectY sensorsBeacons 2000000 None false |> List.length

//printfn $"PART1: {part1}"

let part2Test =
    let maxX = 20
    let searchArea = Some (0, maxX)
    [0..maxX] |> List.collect (fun y -> inspectY sensorsBeacons y searchArea true)
    |> List.head |> fun (x, y) -> x * 4000000 + y
    
let part2 =
    let maxX = 4000000
    let searchArea = Some (0, maxX)
    [0..maxX] |> List.collect (fun y -> inspectY sensorsBeacons y searchArea true)
    |> List.head |> fun (x, y) -> x * 4000000 + y



//printfn $"PART2: {part2}"
