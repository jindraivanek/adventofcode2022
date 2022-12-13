#r "nuget: FParsec"
open FParsec

let lines = System.IO.File.ReadAllLines("day13.test")

let packetPairs =
    lines
    |> Array.chunkBySize 3 |> Array.map (fun xs -> xs[0], xs[1])

type Packet =
    | Number of int
    | List of Packet list

let packetParser() = 
    let digit = satisfy (fun ch -> System.Char.IsDigit ch)
    let number = digit |>> (string >> int >> Number)
    let packet, packetRef = createParserForwardedToRef<Packet, unit>()
    let list = pchar '[' >>. sepBy packet (pchar ',') .>> pchar ']' |>> List
    packetRef := choice [ number; list ]
    packet
let parse str =
    match run (packetParser()) str with
    | Success(result, _, _)   -> 
        printfn "Success: %A" result
        result
    | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

parse "[1,[2,[]],3]"

let part1 = 0
printfn $"PART1: {part1}"

let part2 = 0
printfn $"PART2: {part2}"
