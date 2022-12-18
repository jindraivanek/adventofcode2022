#time
let lines = System.IO.File.ReadAllLines("day18.input")

let cubes =
    lines
    |> Array.map (fun s -> s.Split "," |> fun xs -> int xs[0], int xs[1], int xs[2])
    |> set

let neighbours (x, y, z) =
    [ (x + 1, y, z)
      (x - 1, y, z)
      (x, y + 1, z)
      (x, y - 1, z)
      (x, y, z + 1)
      (x, y, z - 1) ]

let neighboursDiag (x, y, z) =
    [ (x + 1, y + 1, z)
      (x - 1, y + 1, z)
      (x + 1, y - 1, z)
      (x - 1, y - 1, z)
      (x, y + 1, z + 1)
      (x, y + 1, z - 1)
      (x, y - 1, z + 1)
      (x, y - 1, z - 1)
      (x + 1, y, z + 1)
      (x - 1, y, z + 1)
      (x + 1, y, z - 1)
      (x - 1, y, z - 1) ]
    @ neighbours (x, y, z)

let isAdjacent ((x1, y1, z1), (x2, y2, z2)) =
    abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2) = 1

let cubesAround =
    cubes
    |> Seq.collect neighboursDiag
    |> Seq.distinct
    |> Seq.filter (fun x -> Set.contains x cubes |> not)
    |> set

let boundingBox =
    let x1 = cubes |> Seq.map (fun (x, _, _) -> x) |> Seq.min
    let y1 = cubes |> Seq.map (fun (_, y, _) -> y) |> Seq.min
    let z1 = cubes |> Seq.map (fun (_, _, z) -> z) |> Seq.min
    let x2 = cubes |> Seq.map (fun (x, _, _) -> x) |> Seq.max
    let y2 = cubes |> Seq.map (fun (_, y, _) -> y) |> Seq.max
    let z2 = cubes |> Seq.map (fun (_, _, z) -> z) |> Seq.max
    (x1, y1, z1), (x2, y2, z2)

let isInBoudingBox (x, y, z) =
    let (x1, y1, z1), (x2, y2, z2) = boundingBox
    x1 <= x && x <= x2 && y1 <= y && y <= y2 && z1 <= z && z <= z2

let cubesAroundNotInner =
    let start = cubesAround |> Seq.find (isInBoudingBox >> not)

    let rec go acc x =
        let newItems = (x |> neighbours |> set |> Set.intersect cubesAround) - acc
        (Set.add x acc, newItems) ||> Seq.fold go

    go Set.empty start

let part1 = Seq.allPairs cubes cubesAround |> Seq.filter isAdjacent |> Seq.length

printfn $"PART1: {part1}"

let part2 =
    Seq.allPairs cubes cubesAroundNotInner |> Seq.filter isAdjacent |> Seq.length

printfn $"PART2: {part2}"
