#time
let input = System.IO.File.ReadAllText("day17.input")

let jets =
    input
    |> Seq.map (function
        | '<' -> (-1, 0)
        | '>' -> (1, 0))
    |> Seq.toArray

let jetsMod i = i % jets.Length
let jetsInfinite i = jets[i % jets.Length]

let blocks =
    [| set [ 0, 0; 1, 0; 2, 0; 3, 0 ]
       set [ 1, 0; 0, 1; 1, 1; 2, 1; 1, 2 ]
       set [ 2, 0; 2, 1; 0, 2; 1, 2; 2, 2 ]
       set [ 0, 0; 0, 1; 0, 2; 0, 3 ]
       set [ 0, 0; 1, 0; 0, 1; 1, 1 ] |]

let blocksMod i = i % blocks.Length
let blocksInfinite i = blocks[i % blocks.Length]

let posPlus (a, b) (c, d) = a + c, b + d

let moveBlock dir block = block |> Set.map (posPlus dir)

let minX xs = xs |> Seq.map fst |> Seq.min
let maxX xs = xs |> Seq.map fst |> Seq.max
let minY xs = xs |> Seq.map snd |> Seq.min
let maxY xs = xs |> Seq.map snd |> Seq.max

let initBlock b cave =
    let height = if Seq.isEmpty cave then 0 else minY cave
    moveBlock (2, height - 4 - maxY b) b

type State =
    { Step: int
      BlockNumber: int
      Block: Set<int * int> option
      Cave: Set<int * int> }

let simStep
    endCondition
    ({ Step = i
       BlockNumber = j
       Block = block
       Cave = cave } as s)
    =
    if endCondition s then
        None
    else
        match block with
        | None ->
            let topRowToKeep =
                [ 0..6 ]
                |> Seq.map (fun i -> cave |> Seq.filter (fun (x, _) -> x = i) |> Seq.map snd |> Seq.min)
                |> Seq.max

            let topRow = cave |> Seq.map snd |> Seq.min
            let i = s.Step % jets.Length
            let j = s.BlockNumber % blocks.Length

            if i = 2 && j = 0 then
                printfn
                    "%A"
                    (topRow - topRowToKeep,
                     topRowToKeep,
                     s.Step,
                     s.Step % jets.Length,
                     s.BlockNumber,
                     s.BlockNumber % blocks.Length)

            let cave = cave |> Set.filter (fun (_, y) -> y <= topRowToKeep)
            let b = initBlock (blocksInfinite j) cave
            Some({ s with Block = Some b; Cave = cave })
        | Some b ->
            let jet = jetsInfinite i

            let b =
                let b' = moveBlock jet b

                if minX b' < 0 || maxX b' > 6 || Set.intersect b' cave |> Seq.isEmpty |> not then
                    b
                else
                    b'

            let b' = moveBlock (0, 1) b

            if Set.intersect b' cave |> Seq.isEmpty then
                Some({ s with Block = Some b'; Step = i + 1 })
            else
                let s' =
                    { s with
                        Block = None
                        BlockNumber = j + 1
                        Step = i + 1
                        Cave = Set.union cave b }

                Some(s')
        |> Option.map (fun x -> x, x)

let initState =
    { Step = 0
      BlockNumber = 0
      Block = None
      Cave = set [ for i in 0..6 -> i, 0 ] }

let sim endCondition =
    Seq.unfold (simStep endCondition) initState


let part1 =
    sim (fun s -> s.BlockNumber >= 2022) |> Seq.last |> (fun s -> minY s.Cave)

printfn $"PART1: {-part1}"

let part2 () =
    let target = 1000000000000L

    let groups =
        sim (fun s -> s.BlockNumber >= 20000)
        |> Seq.filter (fun s -> s.BlockNumber >= 1000 && blocksMod s.BlockNumber = 0)
        |> Seq.groupBy (fun s -> jetsMod s.Step)
        |> Seq.map snd
        |> Seq.map (Seq.distinctBy (fun s -> s.BlockNumber))

    let g =
        groups
        |> Seq.filter (fun g ->
            (g
             |> Seq.pairwise
             |> Seq.last
             |> fun (a, b) -> (target - int64 b.BlockNumber) % (int64 b.BlockNumber - int64 a.BlockNumber) = 0L))
        |> Seq.head

    let (step, blockStep) =
        g
        |> Seq.map (fun s -> minY s.Cave, s.BlockNumber)
        |> Seq.pairwise
        |> Seq.map (fun ((y1, b1), (y2, b2)) -> y1 - y2, b2 - b1)
        |> Seq.last

    let (startBlocks, startY) =
        g |> Seq.last |> (fun s -> (int64 s.BlockNumber, int64 (minY s.Cave)))

    -startY + ((target - startBlocks) / int64 blockStep) * int64 step

printfn $"PART2: {part2 ()}"
