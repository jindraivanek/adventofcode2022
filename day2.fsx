type RPS =
    | Rock
    | Paper
    | Scissors

let parseShape = function
    | 'A'
    | 'X' -> Rock
    | 'B'
    | 'Y' -> Paper
    | 'C'
    | 'Z' -> Scissors

let evalWin me op =
    match me, op with
    | Rock, Rock
    | Paper, Paper
    | Scissors, Scissors -> 3
    | Rock, Scissors
    | Paper, Rock
    | Scissors, Paper -> 6
    | Scissors, Rock
    | Rock, Paper
    | Paper, Scissors -> 0

let evalShape = function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let evalGame me op = evalWin me op + evalShape me

let lines = System.IO.File.ReadAllLines("day2.input")
let games = lines |> Array.map (fun s -> parseShape s[0], parseShape s[2])
let part1 = games |> Seq.sumBy (fun (op, me) -> evalGame me op)

printfn $"PART1: {part1}"

// PART2

let parseWinScore = function
    | 'X' -> 0
    | 'Y' -> 3
    | 'Z' -> 6

let selectShape opShape winScore =
    [ Rock; Paper; Scissors ] |> List.find (fun x -> evalWin x opShape = winScore)

let games2 = lines |> Array.map (fun s -> parseShape s[0], parseWinScore s[2])
let part2 = games2 |> Seq.sumBy (fun (op, winScore) -> evalGame (selectShape op winScore) op)

printfn $"PART2: {part2}"
