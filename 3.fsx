#load "common.fsx"

open System
open System.Text.RegularExpressions

type NumberPosition = { StartX: int; EndX: int; Y: int }
type SymbolPosition = { X: int; Y: int }

type Symbol = {
    Value: char
    Position: SymbolPosition
}

type Number = { Value: int; Position: NumberPosition }

type Element =
    | Number of Number
    | Symbol of Symbol

let parse rows =
    rows
    |> Array.indexed
    |> Array.collect (fun (index, line) ->
        Regex.Matches(line, "\d+|[^.]")
        |> Seq.map (fun r ->
            match Int32.TryParse r.Value with
            | true, number ->
                Number {
                    Value = number
                    Position = {
                        Y = index
                        StartX = r.Index
                        EndX = r.Index + r.Length - 1
                    }
                }
            | false, _ ->
                Symbol {
                    Value = r.Value[0]
                    Position = { X = r.Index; Y = index }
                })
        |> Seq.toArray)

// Part 1

let isAdjacentTo n s =
    let left = abs (n.StartX - s.X) <= 1
    let right = abs (n.EndX - s.X) <= 1
    let Y = abs (n.Y - s.Y) <= 1
    (left || right) && Y

let split elements =
    elements
    |> Seq.toList
    |> List.partitionMap (function
        | Number x -> Choice1Of2 x
        | Symbol y -> Choice2Of2 y)

let calculatePartNumbersTotal elements =
    let numbers, symbols = split elements

    numbers
    |> List.choose (fun n ->
        let isPartNumber =
            symbols |> List.exists (fun s -> s.Position |> isAdjacentTo n.Position)

        if isPartNumber then Some n else None)
    |> List.sumBy _.Value

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
let file = Files[3] |> parse

file |> calculatePartNumbersTotal

// Part 2
let (|Adjacent|) (numbers: Number list) (position: SymbolPosition) =
    let adjacents =
        numbers |> List.filter (fun n -> position |> isAdjacentTo n.Position)

    Adjacent adjacents

let calculateGearRatio elements =
    let numbers, symbols = split elements

    symbols
    |> List.sumBy (fun symbol ->
        match symbol with
        | {
              Value = '*'
              Position = Adjacent numbers [ a; b ]
          } -> a.Value * b.Value
        | _ -> 0)

file |> calculateGearRatio