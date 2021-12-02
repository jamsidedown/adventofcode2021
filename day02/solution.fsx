open System.IO

let directions = dict ["forward", (1, 0); "down", (0, 1); "up", (0, -1)]

let add (a:int*int) (b:int*int) =
    (fst a + fst b, snd a + snd b)

let parse (input:list<string>) =
    input
    |> List.map (fun line -> line.Split ' ')
    |> List.map (fun line ->
        match line with
        | [| dir; distance |] ->
            let vector = directions[dir]
            let dist = int distance
            (fst vector * dist, snd vector * dist) 
        | _ -> (0, 0))

let parseAim (input:list<int*int>) =
    let rec inner (aim:int) (input:list<int*int>) =
        match input with
        | (d, 0) :: xs -> (d, d * aim) :: inner aim xs
        | (0, a) :: xs -> inner (aim + a) xs
        | _ -> []
    inner 0 input

let partOne (input:list<int*int>) =
    input
    |> List.reduce add
    |> fun (x, y) -> x * y

let partTwo (input:list<int*int>) =
    parseAim input
    |> List.reduce add
    |> fun (x, y) -> x * y

let testInput = ["forward 5"; "down 5"; "forward 8"; "up 3"; "down 8"; "forward 2";]
assert (parse testInput = [(5, 0); (0, 5); (8, 0); (0, -3); (0, 8); (2, 0)])

assert (parse testInput |> partOne = 150)
assert (parse testInput |> partTwo = 900)

let input =
    File.ReadAllLines "day02/input.txt"
    |> Array.toList
    |> parse

printfn $"{partOne input}"
printfn $"{partTwo input}"
