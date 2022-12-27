let lines = System.IO.File.ReadAllLines("day21.input")

open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

type MonkeyId = MonkeyId of string

type Expr =
    | Number of int64
    | Op of MonkeyId * MonkeyId * string * (int64 -> int64 -> int64) * (int64 -> int64 -> int64)

let monkeys =
    lines
    |> Seq.map (function
        | Match "^([a-z]+): ([0-9]+)$" [ m; x ] -> MonkeyId m, Number(int64 x)
        | Match "^([a-z]+): ([a-z]+) ([\+\-\*/]) ([a-z]+)$" [ m; m1; opStr; m2 ] ->
            let op, revOp =
                match opStr with
                | "+" -> (+), (-)
                | "-" -> (-), (+)
                | "*" -> (*), (/)
                | "/" -> (/), (*)

            MonkeyId m, Op(MonkeyId m1, MonkeyId m2, opStr, op, revOp)
        | x -> failwith $"No match {x}")
    |> Map.ofSeq

let rec eval m =
    function
    | Number x -> x
    | Op(m1, m2, opStr, op, _) as x ->
        let r = op (eval m (Map.find m1 m)) (eval m (Map.find m2 m))
        printfn $"{x} : {r}"
        r

type EvalRevResult =
    | NumberResult of int64
    | UnknownFun of (int64 -> int64)

let rec evalRev m =
    let evalHelper mId =
        match mId with
        | MonkeyId "humn" -> UnknownFun id
        | mId -> evalRev m (Map.find mId m)

    function
    | Number x -> NumberResult x
    | Op(m1, m2, opStr, op, revOp) as x ->
        match evalHelper m1, evalHelper m2, opStr with
        | NumberResult x1, NumberResult x2, _ -> NumberResult(op x1 x2)
        | NumberResult x, UnknownFun f, "-"
        | NumberResult x, UnknownFun f, "/" -> UnknownFun(fun y -> f (op x y))
        | NumberResult x, UnknownFun f, _
        | UnknownFun f, NumberResult x, _ -> UnknownFun(fun y -> f (revOp y x))


let part1 = eval monkeys (monkeys[MonkeyId "root"])

printfn $"PART1: {part1}"

let part2 =
    match monkeys[MonkeyId "root"] with
    | Op(m1, m2, _, _, _) ->
        let e1 = evalRev monkeys (monkeys[m1])
        let e2 = evalRev monkeys (monkeys[m2])

        match e1, e2 with
        | UnknownFun f, NumberResult x
        | NumberResult x, UnknownFun f -> f x

printfn $"PART2: {part2}"
