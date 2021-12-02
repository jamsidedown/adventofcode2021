open System.IO

let testInput = ["forward 5"; "down 5"; "forward 8"; "up 3"; "down 8"; "forward 2";]

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
        | _ ->
            printfn "Something fucked up"
            (0, 0))

assert (parse testInput = [(5, 0); (0, 5); (8, 0); (0, -3); (0, 8); (2, 0)])

let partOne (input:list<int*int>) =
    input
    |> List.reduce add
    |> fun (x, y) -> x * y

assert (parse testInput |> partOne = 150)

let input =
    File.ReadAllLines "day02/input.txt"
    |> Array.toList
    |> parse

printfn $"{partOne input}"
