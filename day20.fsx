let lines = System.IO.File.ReadAllLines("day20.input")
let arr () = lines |> Array.map int64

let mixing repeat (inputF: unit -> array<int64>) =
    let input = inputF ()
    let arr = input |> Array.indexed |> Array.map fst

    let swap i j =
        let tmp = arr[i]
        arr[i] <- arr[j]
        arr[j] <- tmp

    let rec move i k =
        let p i =
            i % arr.Length |> fun y -> if y < 0 then y + arr.Length else y

        let x = arr[i]

        if k = 0 then
            ()
        elif k > 0 then
            swap i (p (i + 1))
            move (p (i + 1)) (k - 1)
        else
            swap (p (i - 1)) i
            move (p (i - 1)) (k + 1)

    let items = arr |> Seq.toList

    for _ in 1..repeat do
        items
        |> Seq.iter (fun x ->
            let i = arr |> Array.findIndex ((=) x)
            move i (input[x] % (arr.LongLength - 1L) |> int))

    arr |> Array.map (fun x -> input[x])

let getResult r =
    let indexes = [ 1000; 2000; 3000 ]
    let zeroIndex = r |> Array.findIndex ((=) 0L)
    let values = indexes |> Seq.map (fun i -> r[(i + zeroIndex) % r.Length])
    values |> Seq.sum


let part1 =
    let r = mixing 1 arr
    getResult r

printfn $"PART1: {part1}"

let part2 =
    let mult = 811589153L
    let r = mixing 10 (fun () -> arr () |> Array.map (fun x -> int64 x * mult))
    getResult r

printfn $"PART2: {part2}"
