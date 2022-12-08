let lines = System.IO.File.ReadAllLines("day8.input")
let n = Array.length lines
let grid = Array2D.init n n (fun i j -> lines[i][j] |> string |> int)

let directionsToEdge x y grid =
    [ seq { 0 .. x - 1 } |> Seq.map (fun i -> Array2D.get grid i y)
      seq { 0 .. y - 1 } |> Seq.map (fun j -> Array2D.get grid x j)
      seq { x + 1 .. n - 1 } |> Seq.map (fun i -> Array2D.get grid i y)
      seq { y + 1 .. n - 1 } |> Seq.map (fun j -> Array2D.get grid x j) ]

let mapDirections x y f grid =
    let k = Array2D.get grid x y
    directionsToEdge x y grid |> List.map (fun xs -> f k xs)

let isVisible x y grid =
    grid
    |> mapDirections x y (fun k xs -> xs |> Seq.forall (fun x -> x < k))
    |> List.exists id

let scenicScore x y grid =
    grid
    |> mapDirections x y (fun k xs ->
        if Seq.isEmpty xs then
            0
        else
            xs |> Seq.takeWhile (fun x -> x < k) |> Seq.length |> (+) 1)
    |> List.reduce (*)

let part1 =
    seq { 0 .. n - 1 }
    |> Seq.collect (fun i -> seq { 0 .. n - 1 } |> Seq.filter (fun j -> isVisible i j grid))
    |> Seq.length

printfn $"PART1: {part1}"

let part2 =
    seq { 0 .. n - 1 }
    |> Seq.collect (fun i -> seq { 0 .. n - 1 } |> Seq.map (fun j -> scenicScore i j grid))
    |> Seq.max

printfn $"PART2: {part2}"
