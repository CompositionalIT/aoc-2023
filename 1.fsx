#load "common.fsx"
open System

// Part 1
let processLine (line: string) =
    let digits = line |> Seq.filter Char.IsDigit |> Seq.map Char.GetNumericValue
    $"{Seq.head digits}{Seq.last digits}" |> Convert.ToInt32

let sample =
    """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"""

sample.ByNewLine() |> Seq.sumBy processLine

Files[1] |> Seq.sumBy processLine

// Part 2
let words =
    [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
    |> List.indexed
    |> List.map (fun (index, word) -> {| Text = word; Value = index + 1 |})

let processLineSmart (line: string) =
    let digits =
        line
        |> Seq.indexed
        |> Seq.filter (snd >> Char.IsDigit)
        |> Seq.map (fun (pos, c) -> {|
            Position = pos
            Value = Char.GetNumericValue c |> int
        |})
        |> Seq.toList

    let words =
        words
        |> List.collect (fun word ->
            let matchingWords =
                [
                    {|
                        Value = word.Value
                        Position = line.IndexOf word.Text
                    |}
                    {|
                        Value = word.Value
                        Position = line.LastIndexOf word.Text
                    |}
                ]
                |> List.filter (fun w -> w.Position > -1)
                |> List.sortBy _.Position

            match matchingWords with
            | [] -> []
            | matchingWords -> [ List.head matchingWords; List.last matchingWords ])
        |> List.distinct

    let combined = digits @ words |> List.sortBy _.Position |> List.map _.Value

    $"{List.head combined}{List.last combined}" |> Convert.ToInt32

let sampleHard =
    """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"""

sampleHard.ByNewLine() |> Seq.sumBy processLineSmart

Files[1] |> Seq.sumBy processLineSmart