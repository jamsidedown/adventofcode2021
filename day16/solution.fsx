open System
open System.IO

let conversions = Map [
    '0', "0000"; '1', "0001"; '2', "0010"; '3', "0011"; '4', "0100"; '5', "0101"; '6', "0110"; '7', "0111";
    '8', "1000"; '9', "1001"; 'A', "1010"; 'B', "1011"; 'C', "1100"; 'D', "1101"; 'E', "1110"; 'F', "1111"
]

type PacketRecord<'a> = { version:int; pType:int; value:'a }

type Packet =
    | Literal of PacketRecord<int64>
    | Operator of PacketRecord<list<Packet>>

let toRecord (version:int) (pType:int) (value:'a) = { version = version; pType = pType; value = value}

let tostring (input:list<char>) =
    input |> List.map string |> String.concat ""

let parseHex (input:string) =
    input
    |> Seq.collect (fun c -> conversions[c])
    |> Seq.toList

let binToInt (input:list<char>) =
    input
    |> List.map string
    |> String.concat ""
    |> fun x -> Convert.ToInt32(x, 2)

let binToLong (input:list<char>) =
    input
    |> List.map string
    |> String.concat ""
    |> fun x -> Convert.ToInt64(x, 2)

let rec parseLiteral (input:list<char>) =
    match input with
    | '1' :: a :: b :: c :: d :: ls ->
        let (value, tail) = parseLiteral ls
        (a :: b :: c :: d :: value, tail)
    | '0' :: a :: b :: c :: d :: ls -> ([ a; b; c; d ], ls)
    | ls -> ([], ls)

let rec resolveOperator (packet:Packet) =
    match packet with
    | Literal _ -> Some packet
    | Operator packetRecord ->
        let values =
            packetRecord.value
            |> List.map (fun p ->
                match p with
                | Literal _ -> Some p
                | Operator _ -> resolveOperator p)
            |> List.choose id
            |> List.map (fun p ->
                match p with
                | Literal pr -> Some pr.value
                | _ -> None)
            |> List.choose id
        let toThisRecord (value:int64) = value |> toRecord packetRecord.version packetRecord.pType |> Literal
        match packetRecord.pType with
        | 0 -> values |> List.sum |> toThisRecord |> Some
        | 1 -> values |> List.reduce (*) |> toThisRecord |> Some
        | 2 -> values |> List.min |> toThisRecord |> Some
        | 3 -> values |> List.max |> toThisRecord |> Some
        | 5 ->
            match values with
            | [ a; b ] -> (if a > b then 1L else 0L) |> toThisRecord |> Some
            | _ -> None
        | 6 ->
            match values with
            | [ a; b ] -> (if a < b then 1L else 0L) |> toThisRecord |> Some
            | _ -> None
        | 7 ->
            match values with
            | [ a; b ] -> (if a = b then 1L else 0L) |> toThisRecord |> Some
            | _ -> None
        | _ -> None

let rec parsePacket (input:list<char>) =
    match input with
    | a :: b :: c :: d :: e :: f :: ls ->
        let packetVersion = [ a; b; c ] |> binToInt
        let packetType = [ d; e; f ] |> binToInt
        match packetType with
        | 4 ->
            let (bits, tail) = parseLiteral ls
            let (packets, tail) = parsePacket tail
            let literal = bits |> binToLong |> toRecord packetVersion packetType |> Literal
            (literal :: packets, tail)
        | _ ->
            match ls with
            | '0' :: ls when ls.Length > 15 ->
                let length = ls |> List.take 15 |> binToInt
                let (packets, _) = ls |> List.skip 15 |> List.take length |> parsePacket
                let tail = ls |> List.skip 15 |> List.skip length
                let operator = packets |> toRecord packetVersion packetType |> Operator
                let (packets, tail) = parsePacket tail
                (operator :: packets, tail)
            | '1' :: ls when ls.Length > 11 ->
                let count = ls |> List.take 11 |> binToInt
                let (packets, tail) = ls |> List.skip 11 |> parsePacket
                let operator = packets |> List.take count |> toRecord packetVersion packetType |> Operator
                let packets = packets |> List.skip count
                (operator :: packets, tail)
            | _ -> ([], [])
    | _ -> ([], [])

let partOne (input:string) =
    let rec recurse (packet:Packet) =
        match packet with
        | Literal r -> r.version
        | Operator r ->
            let sum = r.value |> List.sumBy (fun p -> recurse p)
            sum + r.version

    input
    |> parseHex
    |> parsePacket
    |> fst
    |> List.head
    |> recurse

let partTwo (input:string) =
    input
    |> parseHex
    |> parsePacket
    |> fst
    |> List.head
    |> resolveOperator
    |> function
        | Some (Literal p) -> p.value
        | _ -> 999L

assert (partOne "8A004A801A8002F478" = 16)
assert (partOne "620080001611562C8802118E34" = 12)
assert (partOne "C0015000016115A2E0802F182340" = 23)
assert (partOne "A0016C880162017C3686B18A3D4780" = 31)

assert (partTwo "C200B40A82" = 3)
assert (partTwo "04005AC33890" = 54)
assert (partTwo "880086C3E88112" = 7)
assert (partTwo "CE00C43D881120" = 9)
assert (partTwo "D8005AC2A8F0" = 1)
assert (partTwo "F600BC2D8F" = 0)
assert (partTwo "9C005AC2F8F0" = 0)
assert (partTwo "9C0141080250320F1802104A08" = 1)

let input = File.ReadAllText "day16/input.txt" |> fun s -> s.TrimEnd()
printfn $"{partOne input}"
printfn $"{partTwo input}"
