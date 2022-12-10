let lines = System.IO.File.ReadAllLines("day10.input")

let instructions =
    lines
    |> Seq.map (fun s -> s.Split " " |> fun xs -> xs[0], xs |> Array.tryItem 1 |> Option.map int)
    |> Seq.toList

type CPU = { Cycle: int; X: int }

let normalizeInstr =
    function
    | "noop", _ as x -> [ x ]
    | "addx", _ as x -> [ "noop", None; x ]

let doInstr cpu =
    let noop = { cpu with Cycle = cpu.Cycle + 1 }

    function
    | "noop", None -> noop
    | "addx", Some x -> { noop with X = cpu.X + x }

let normalizedInstrs = instructions |> List.collect normalizeInstr

let init = { Cycle = 1; X = 1 }
let cpuStates = (init, normalizedInstrs) ||> List.scan doInstr

let computeSignalStrength afterCycle =
    cpuStates
    |> List.filter (fun x -> afterCycle x.Cycle)
    |> List.map (fun x -> x.Cycle * x.X)
    |> List.sum

let display =
    cpuStates
    |> List.map (fun x -> if abs (x.X - ((x.Cycle - 1) % 40)) <= 1 then "#" else ".")

let draw display =
    display
    |> List.chunkBySize 40
    |> List.map (String.concat "")
    |> List.iter (printfn "%s")

let part1 = computeSignalStrength (fun c -> (c - 20) % 40 = 0)
printfn $"PART1: {part1}"

printfn $"PART2:"
draw display
