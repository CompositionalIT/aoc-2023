#load "common.fsx"

open System
open System.Text.RegularExpressions

type NumberPosition = { StartX: int; EndX: int; Y: int }
type SymbolPosition = { X: int; Y: int }
type Number = { Value: int; Position: NumberPosition }

type Symbol = {
    Value: char
    Position: SymbolPosition
}

let (|Number|_|) (s: string) =
    match Int32.TryParse s with
    | true, number -> Some number
    | false, _ -> None

let (|Char|_|) (s: string) =
    match s.Length with
    | 1 -> Some s[0]
    | _ -> None

let parse rows =
    rows
    |> Array.indexed
    |> Array.collect (fun (index, line) ->
        Regex.Matches(line, "\d+|[^.]")
        |> Seq.map (fun m -> {|
            Y = index
            X = m.Index
            Value = m.Value
        |})
        |> Seq.toArray)
    |> Array.toList
    |> List.partitionMap (fun row ->
        match row.Value with
        | Number n ->
            Choice1Of2(
                {
                    Number.Value = n
                    Position = {
                        Y = row.Y
                        StartX = row.X
                        EndX = row.X + row.Value.Length - 1
                    }
                }
            )
        | Char symbol ->
            Choice2Of2(
                {
                    Value = symbol
                    Position = { X = row.X; Y = row.Y }
                }
            )
        | _ -> failwith "Invalid input")

let isAdjacentTo n s =
    let left = abs (n.StartX - s.X) <= 1
    let right = abs (n.EndX - s.X) <= 1
    let Y = abs (n.Y - s.Y) <= 1
    (left || right) && Y

let sample =
    """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""

let parsedSample = sample.ByNewLine() |> parse
let parsedFile = Files[3] |> parse

// Part 1
let calculatePartNumbersTotal (numbers: Number list, symbols) =
    numbers
    |> List.choose (fun n ->
        let isPartNumber =
            symbols |> List.exists (fun s -> s.Position |> isAdjacentTo n.Position)

        if isPartNumber then Some n else None)
    |> List.sumBy _.Value

parsedSample |> calculatePartNumbersTotal
parsedFile |> calculatePartNumbersTotal

// Part 2
let (|AdjacentTo|) (numbers: Number list) position =
    let adjacents =
        numbers |> List.filter (fun n -> position |> isAdjacentTo n.Position)

    AdjacentTo adjacents

let calculateGearRatio (numbers, symbols) =
    symbols
    |> List.sumBy (fun symbol ->
        match symbol with
        | {
              Value = '*'
              Position = AdjacentTo numbers [ a; b ]
          } -> a.Value * b.Value
        | _ -> 0)

parsedSample |> calculateGearRatio
parsedFile |> calculateGearRatio