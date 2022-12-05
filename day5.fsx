let stacks, moves =
    let lines = System.IO.File.ReadAllLines("day5.input")
    let sepIndex = lines |> Array.findIndex Seq.isEmpty
    let linesStacks = seq { 0 .. sepIndex - 2 } |> Seq.map (fun i -> lines[i])

    let linesMoves =
        seq { sepIndex + 1 .. Array.length lines - 1 } |> Seq.map (fun i -> lines[i])

    let stacks =
        linesStacks
        |> Seq.map (fun s -> s |> Seq.chunkBySize 4 |> Seq.map (fun x -> x[1]) |> Seq.toArray)
        |> Array.transpose
        |> Array.map (Array.filter ((<>) ' ') >> Array.toList)

    let moves =
        let re =
            System.Text.RegularExpressions.Regex "move ([0-9]+) from ([0-9]+) to ([0-9]+)"

        linesMoves
        |> Seq.map (fun s -> let g = re.Match(s).Groups in int g[1].Value, int g[2].Value, int g[3].Value)

    stacks, moves

let applyMoves moves =
    (stacks, moves)
    ||> Seq.fold (fun s (count, from, target) ->
        s
        |> Array.mapi (fun i _ ->
            if i = from - 1 then
                List.skip count s[i]
            elif i = target - 1 then
                List.take count s[from - 1] @ s[target - 1]
            else
                s[i]))

let getResult stacks =
    stacks |> Array.map List.head |> Seq.map string |> String.concat ""

let afterMoves1 =
    applyMoves (
        moves
        |> Seq.collect (fun (count, from, target) -> Seq.replicate count (1, from, target))
    )

let part1 = getResult afterMoves1
printfn $"PART1: {part1}"

let afterMoves2 = applyMoves moves
let part2 = getResult afterMoves2
printfn $"PART2: {part2}"
