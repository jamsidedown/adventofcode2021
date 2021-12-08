open System.IO

let readInput (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map (fun line ->
        match line.Split " | " with
        | [| a; b |] -> Some (a.Split ' ', b.Split ' ')
        | _ -> None)
    |> Array.choose id

let partOne (input:array<array<string>*array<string>>) =
    let segmentCounts = Set.ofList [ 2; 3; 4; 7 ]

    input
    |> Array.map snd
    |> Array.sumBy (fun line ->
        line
        |> Array.filter (fun num ->
            match num.Length with
            | n when segmentCounts.Contains n -> true
            | _ -> false)
        |> Array.length)

let solveLine (line:array<string>*array<string>) =
    let words = fst line |> Array.map Set.ofSeq

    let one = words |> Array.filter (fun word -> word.Count = 2) |> Array.head
    let four = words |> Array.filter (fun word -> word.Count = 4) |> Array.head
    let seven = words |> Array.filter (fun word -> word.Count = 3) |> Array.head
    let eight = words |> Array.filter (fun word -> word.Count = 7) |> Array.head

    let zeroSixNine = words |> Array.filter (fun word -> word.Count = 6)
    let twoThreeFive = words |> Array.filter (fun word -> word.Count = 5)

    let three = twoThreeFive |> Array.filter (Set.isSubset one) |> Array.head
    let nine = zeroSixNine |> Array.filter (Set.isSubset three) |> Array.head

    let twoFive = twoThreeFive |> Array.filter (fun word -> word <> three)
    let five = twoFive |> Array.filter (Set.isSuperset nine) |> Array.head
    let two = twoFive |> Array.filter (fun word -> word <> five) |> Array.head

    let zeroSix = zeroSixNine |> Array.filter (fun word -> word <> nine)
    let zero = zeroSix |> Array.filter (Set.isSubset one) |> Array.head
    let six = zeroSix |> Array.filter (fun word -> word <> zero) |> Array.head

    let lookup =
        [(zero, "0"); (one, "1"); (two, "2"); (three, "3"); (four, "4");
        (five, "5"); (six, "6"); (seven, "7"); (eight, "8"); (nine, "9")] |> dict

    snd line
    |> Array.map Set.ofSeq
    |> Array.map (fun s -> lookup[s])
    |> String.concat ""
    |> int

let partTwo (input:array<array<string>*array<string>>) =
    input |> Array.sumBy solveLine

let testInput = readInput "day08/testInput.txt"
assert (partOne testInput = 26)
assert (partTwo testInput = 61229)

let input = readInput "day08/input.txt"
printfn $"{partOne input}"
printfn $"{partTwo input}"
