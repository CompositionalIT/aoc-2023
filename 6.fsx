#load "common.fsx"
open System

let sample =
    """Time:      7  15   30
Distance:  9  40  200"""

[<Measure>]
type Ms

[<Measure>]
type Mm

let parse postProcessor lines =
    lines
    |> Array.map (fun line ->
        match line with
        | Split ':' [ category; Split ' ' values ] -> category, values
        | _ -> failwith "Invalid input")
    |> function
        | [| "Time", times; "Distance", distances |] ->
            let distances = distances |> postProcessor
            let times = times |> postProcessor

            List.zip distances times
            |> List.map (fun (d, t) -> {|
                Distance = d * 1UL<Mm>
                Duration = t * 1UL<Ms>
            |})
        | _ -> failwith "Invalid input"

let processTime
    (time:
        {|
            Distance: uint64<Mm>
            Duration: uint64<Ms>
        |})
    =
    [
        for buttonTime in 1UL .. (uint64 time.Duration - 1UL) do
            let speed = buttonTime * 1UL<Mm / Ms>
            let buttonTime = buttonTime * 1UL<Ms>

            if ((time.Duration - buttonTime) * speed) > time.Distance then
                buttonTime
    ]
    |> List.length
// Part 1
let part1Parser = parse (List.map uint64)
let parsedSample = sample.ByNewLine() |> part1Parser
let parsedData = Files[6] |> part1Parser

let partOne
    (times:
        {|
            Distance: uint64<Mm>
            Duration: uint64<Ms>
        |} list)
    =
    times |> List.map processTime |> List.reduce (*)

parsedSample |> partOne
parsedData |> partOne

// Part 2
let part2Parser = parse (List.reduce (+) >> uint64 >> List.singleton)
let parsedSamplePart2 = sample.ByNewLine() |> part2Parser |> List.head
let parsedFilePart2 = Files[6] |> part2Parser |> List.head
processTime parsedSamplePart2
processTime parsedFilePart2