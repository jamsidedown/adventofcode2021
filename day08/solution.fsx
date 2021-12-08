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
    let one = fst line |> Array.filter (fun word -> word.Length = 2) |> Array.head |> Set.ofSeq
    let seven = fst line |> Array.filter (fun word -> word.Length = 3) |> Array.head |> Set.ofSeq
    let four = fst line |> Array.filter (fun word -> word.Length = 4) |> Array.head |> Set.ofSeq
    let eight = fst line |> Array.filter (fun word -> word.Length = 7) |> Array.head |> Set.ofSeq

    let topSegment = Set.difference seven one |> Set.toArray |> Array.head

    let zeroSixNine = fst line |> Array.filter (fun word -> word.Length = 6) |> Array.map Set.ofSeq
    let nine =
        zeroSixNine
        |> Array.filter (fun word -> Set.difference word (Set.add topSegment four) |> Set.count = 1)
        |> Array.head

    let zeroSix = zeroSixNine |> Array.filter (fun word -> word <> nine)
    let zero = zeroSix |> Array.filter (fun word -> Set.difference one word |> Set.count = 0) |> Array.head
    let six = zeroSix |> Array.filter (fun word -> word <> zero) |> Array.head

    let bottomLeftSegment = Set.difference eight nine |> Set.toArray |> Array.head
    let topRightSegment = Set.difference eight six |> Set.toArray |> Array.head
    let bottomRightSegment = one |> Set.remove topRightSegment |> Set.toArray |> Array.head

    let twoThreeFive = fst line |> Array.filter (fun word -> word.Length = 5) |> Array.map Set.ofSeq
    let five = eight |> Set.remove bottomLeftSegment |> Set.remove topRightSegment

    let twoThree = twoThreeFive |> Array.filter (fun word -> word <> five)
    let three = twoThree |> Array.filter (fun word -> word.Contains bottomRightSegment) |> Array.head
    let two = twoThree |> Array.filter (fun word -> word <> three) |> Array.head

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
