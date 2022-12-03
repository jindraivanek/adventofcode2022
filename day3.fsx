let lines = System.IO.File.ReadAllLines("day3.input")

let getCompartments s =
    let n = String.length s
    let c1 = s |> Seq.take (n / 2) |> set
    let c2 = s |> Seq.skip (n / 2) |> set
    c1, c2

let itemPriority (c: char) =
    if c >= 'a' then
        int c - int 'a' + 1
    else
        int c - int 'A' + 27

let part1 =
    lines
    |> Seq.collect (fun s -> getCompartments s ||> Set.intersect)
    |> Seq.sumBy itemPriority

printfn $"PART1: {part1}"

let part2 =
    lines
    |> Seq.map set
    |> Seq.chunkBySize 3
    |> Seq.collect Set.intersectMany
    |> Seq.sumBy itemPriority

printfn $"PART2: {part2}"
