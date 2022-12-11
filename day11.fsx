let maxWorry = 2L * 3L * 5L * 7L * 11L * 13L * 17L * 19L * 23L // hardcoded by max prime in inputs
let lines = System.IO.File.ReadAllLines("day11.input")
let splittedLines = lines |> Array.chunkBySize 7

open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

type Monkey =
    { Items: int64 list
      Operation: int64 -> int64
      Test: int64 -> int // itemWorry -> monkeyNumber
      InspectionCount: int }

let readMonkey relief (arr: string[]) =
    let items =
        match arr[1] with
        | Match ".*: (.*)" [ s ] -> s.Split ", " |> Seq.map int64 |> Seq.toList

    let operation =
        match arr[2] with
        | Match ".* old \* old" [] -> fun x -> x * x
        | Match ".* old \* ([0-9]*)" [ s ] -> let y = int64 s in fun x -> x * y
        | Match ".* old \+ ([0-9]*)" [ s ] -> let y = int64 s in fun x -> x + y
        |> fun op ->
            if relief then
                fun x -> op x / 3L
            else
                fun x -> op x % maxWorry

    let test =
        let condition =
            match arr[3] with
            | Match ".* divisible by ([0-9]*)" [ s ] -> let d = int64 s in fun x -> x % d = 0L

        let trueMonkey =
            match arr[4] with
            | Match ".* If true: throw to monkey ([0-9]*)" [ s ] -> int s

        let falseMonkey =
            match arr[5] with
            | Match ".* If false: throw to monkey ([0-9]*)" [ s ] -> int s

        fun x -> if condition x then trueMonkey else falseMonkey

    { Items = items
      Operation = operation
      Test = test
      InspectionCount = 0 }

let monkeyBusiness relief rounds =
    let monkeys =
        splittedLines |> Array.map (readMonkey relief) |> Seq.indexed |> Map.ofSeq

    let doRound monkeys =
        (monkeys, Map.keys monkeys |> Seq.sort)
        ||> Seq.fold (fun s i ->
            let monkey = Map.find i s

            let itemChanges =
                monkey.Items
                |> List.map (fun w ->
                    let newWorry = monkey.Operation w
                    let newMonkeyNumber = monkey.Test newWorry
                    newMonkeyNumber, newWorry)

            (s, itemChanges)
            ||> Seq.fold (fun s (m, w) -> s |> Map.add m { s[m] with Items = s[m].Items @ [ w ] })
            |> Map.add
                i
                { monkey with
                    InspectionCount = monkey.InspectionCount + List.length monkey.Items
                    Items = [] })

    let allRounds = (monkeys, [ 1..rounds ]) ||> List.scan (fun s _ -> doRound s)

    let inspections =
        allRounds |> Seq.last |> Map.map (fun _ x -> x.InspectionCount) |> Map.toSeq

    inspections
    |> Seq.map snd
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.toArray
    |> fun xs -> int64 xs[0] * int64 xs[1]

let part1 = monkeyBusiness true 20

printfn $"PART1: {part1}"

let part2 = monkeyBusiness false 10000

printfn $"PART2: {part2}"
