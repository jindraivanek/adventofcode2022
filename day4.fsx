let lines = System.IO.File.ReadAllLines("day4.input")
let pair (xs: array<_>) = xs[0], xs[1]

let ranges =
    lines
    |> Array.map (fun s -> s.Split ',' |> Array.map (fun x -> x.Split '-' |> Array.map int |> pair) |> pair)

let contains (l1, r1) (l2, r2) = l1 <= l2 && r2 <= r1
let overlaps (l1, r1) (l2, r2) = max l1 l2 <= min r1 r2

let part1 =
    ranges
    |> Seq.sumBy (fun (r1, r2) -> if contains r1 r2 || contains r2 r1 then 1 else 0)

printfn $"PART1: {part1}"

let part2 = ranges |> Seq.sumBy (fun (r1, r2) -> if overlaps r1 r2 then 1 else 0)
printfn $"PART2: {part2}"
