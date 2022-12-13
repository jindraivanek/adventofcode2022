#time
#r "nuget: FParsec"

open FParsec

let lines = System.IO.File.ReadAllLines("day13.input")

type Packet =
    | Number of int
    | List of Packet list

let packetParser () =
    let digit = satisfy (fun ch -> System.Char.IsDigit ch)
    let number = many1 digit |>> (List.map string >> String.concat "" >> int >> Number)
    let packet, packetRef = createParserForwardedToRef<Packet, unit> ()
    let list = pchar '[' >>. sepBy packet (pchar ',') .>> pchar ']' |>> List
    packetRef.Value <- choice [ number; list ]
    packet

let parse str =
    match run (packetParser ()) str with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

let packetPairs =
    lines |> Array.chunkBySize 3 |> Array.map (fun xs -> parse xs[0], parse xs[1])

type Ordering =
    | Right
    | Undecided
    | NotRight

let rec packetOrder =
    function
    | Number a, Number b when a < b -> Right
    | Number a, Number b when a > b -> NotRight
    | Number a, Number b when a = b -> Undecided
    | List(a :: ta), List(b :: tb) ->
        match packetOrder (a, b) with
        | Right -> Right
        | NotRight -> NotRight
        | Undecided -> packetOrder (List ta, List tb)
    | List [], List(_ :: _) -> Right
    | List(_ :: _), List [] -> NotRight
    | List [], List [] -> Undecided
    | a, Number b -> packetOrder (a, List [ Number b ])
    | Number a, b -> packetOrder (List [ Number a ], b)

let rightPacketPairs =
    packetPairs
    |> Seq.indexed
    |> Seq.filter (fun (_, p) -> packetOrder p = Right)
    |> Seq.map (fst >> (+) 1)
    |> Seq.toList

let divider2 = List[List[Number 2]]
let divider6 = List[List[Number 6]]

let packetsWithDividers =
    packetPairs
    |> Seq.collect (fun (x, y) -> [ x; y ])
    |> fun xs -> divider2 :: divider6 :: Seq.toList xs

let orderedPackets =
    packetsWithDividers
    |> Seq.sortWith (fun x y ->
        packetOrder (x, y)
        |> (function
        | Right -> -1
        | NotRight -> 1
        | Undecided -> 0))
    |> Seq.toList

let part1 = rightPacketPairs |> Seq.sum
printfn $"PART1: {part1}"

let part2 =
    (List.findIndex ((=) divider2) orderedPackets + 1)
    * (List.findIndex ((=) divider6) orderedPackets + 1)

printfn $"PART2: {part2}"
