let lines = System.IO.File.ReadAllLines("day9.input")

let instructions =
    lines
    |> Seq.collect (fun s -> s.Split " " |> fun xs -> Seq.replicate (int xs[1]) xs[0])
    |> Seq.toList

let posPlus (a, b) (c, d) = a + c, b + d
let posMinus (a, b) (c, d) = a - c, b - d

let moveDir =
    function
    | "R" -> 1, 0
    | "L" -> -1, 0
    | "U" -> 0, 1
    | "D" -> 0, -1
    | _ -> 0, 0

let knotMove head tail =
    let (x, y) = posMinus head tail

    let diag =
        if x <> 0 && y <> 0 && (abs x > 1 || abs y > 1) then
            1
        else
            0

    let follow x =
        if x > 1 - diag then 1
        elif x < -1 + diag then -1
        else 0

    follow x, follow y

let initKnots n = List.replicate n (0, 0)

let knotsMoves n =
    (initKnots n, instructions)
    ||> List.scan (fun (head :: knots) dir ->
        let h = posPlus head (moveDir dir)

        let movedKnots =
            (h, knots) ||> List.scan (fun k1 k2 -> posPlus k2 (knotMove k1 k2)) |> List.tail

        h :: movedKnots)

let part1 = knotsMoves 2 |> List.map List.last |> List.distinct |> List.length
printfn $"PART1: {part1}"

let part2 = knotsMoves 10 |> List.map List.last |> List.distinct |> List.length

printfn $"PART2: {part2}"
