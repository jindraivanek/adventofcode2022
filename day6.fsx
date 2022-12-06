let text = System.IO.File.ReadAllText("day6.input") |> Seq.toArray

let getMarkerIndex markerSize =
    text
    |> Seq.windowed markerSize
    |> Seq.indexed
    |> Seq.find (fun (_, xs) -> xs |> Seq.distinct |> Seq.length = markerSize)
    |> fst
    |> fun i -> i + markerSize

let part1 = getMarkerIndex 4
printfn $"PART1: {part1}"

let part2 = getMarkerIndex 14
printfn $"PART2: {part2}"
