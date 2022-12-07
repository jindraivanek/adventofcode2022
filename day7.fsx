let lines = System.IO.File.ReadAllLines("day7.input")

open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

module Map =
    let updateWithDef key def f (m: Map<_, _>) =
        m
        |> Map.tryFind key
        |> Option.defaultValue def
        |> f
        |> fun x -> Map.add key x m

type Path = Path of list<string> // dir path in reverse order

let rec allSubPaths: Path -> list<Path> =
    function
    | Path [] -> [ Path [] ]
    | Path(_ :: xs) as p -> p :: allSubPaths (Path xs)

let folder (Path curDir, dirSizes) command =
    match command with
    | "$ cd /" -> Path [], dirSizes
    | "$ cd .." -> Path(List.tail curDir), dirSizes
    | Match "\$ cd (.*)" [ d ] -> Path(d :: curDir), dirSizes
    | Match "([0-9]+) .*" [ sizeString ] ->
        let size = int sizeString

        let newDirSizes =
            (dirSizes, allSubPaths (Path curDir))
            ||> Seq.fold (fun m path -> Map.updateWithDef path 0 ((+) size) m)

        Path curDir, newDirSizes
    | _ -> Path curDir, dirSizes

let totalDirSizes = ((Path [], Map.empty), lines) ||> Seq.fold folder |> snd

let part1 =
    totalDirSizes |> Map.filter (fun _ x -> x <= 100000) |> Map.values |> Seq.sum

printfn $"PART1: {part1}"

let part2 =
    let toFree = 30000000 - (70000000 - totalDirSizes[Path []])
    totalDirSizes |> Map.filter (fun _ x -> x >= toFree) |> Map.values |> Seq.min

printfn $"PART1: {part2}"
