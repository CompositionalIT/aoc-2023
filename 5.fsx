#load "common.fsx"

open System
open System.Collections.Generic

type Category = Category of string

type ElementMap = {
    Source: Category
    Destination: Category
    Ranges:
        {|
            DestinationStart: uint64
            SourceStart: uint64
            Length: uint64
            SourceEnd: uint64
            Distance: uint64
        |} array
} with

    member this.Convert from =
        this.Ranges
        |> Array.tryFind (fun range -> from >= range.SourceStart && from < range.SourceEnd)
        |> Option.map (fun range -> range.Distance + from)
        |> Option.defaultValue from

type Almanac = {
    Seeds: uint64 list
    Maps: IReadOnlyDictionary<Category, ElementMap>
}

let parse rows =
    ({|
        CurrentMap = None
        Output = {| Seeds = []; Maps = [] |}
     |},
     rows)
    ||> Array.fold (fun state line ->
        match line, state.CurrentMap with
        | Split ':' [ "seeds"; Split ' ' seeds ], _ -> {|
            state with
                Output = {|
                    state.Output with
                        Seeds = seeds |> List.map uint64
                |}
          |}
        | Split ' ' [ Split '-' [ fromMap; "to"; toMap ]; "map:" ], _ -> {|
            state with
                CurrentMap =
                    Some {
                        Source = Category fromMap
                        Destination = Category toMap
                        Ranges = Array.empty
                    }
          |}
        | Split ' ' [ UInt64 destination; UInt64 start; UInt64 length ], Some currentMap -> {|
            state with
                CurrentMap =
                    Some {
                        currentMap with
                            Ranges =
                                Array.append
                                    currentMap.Ranges
                                    (Array.singleton {|
                                        DestinationStart = destination
                                        Distance = destination - start
                                        SourceStart = start
                                        SourceEnd = start + length
                                        Length = length
                                    |})

                    }
          |}
        | "", Some currentMap -> {|
            state with
                CurrentMap = None
                Output = {|
                    state.Output with
                        Maps = currentMap :: state.Output.Maps
                |}
          |}
        | _ -> state)
    |> fun state -> {
        Seeds = state.Output.Seeds
        Maps =
            (match state.CurrentMap with
             | Some currentMap -> currentMap :: state.Output.Maps
             | None -> state.Output.Maps)
            |> List.map (fun map -> map.Source, map)
            |> readOnlyDict
    }

let sample =
    """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"""

let parsedSample = sample.ByNewLine() |> parse
let parsedFile = Files[5] |> parse

// This is too slow for large numbers of calls. What's a better data structure to use to store the location maps instead of an array / list?
let makeSeedToLocation almanac =
    let rec composeMapConverters category output =
        if almanac.Maps.ContainsKey category then
            let newFunc = output >> almanac.Maps[category].Convert
            composeMapConverters almanac.Maps[category].Destination newFunc
        else
            output

    composeMapConverters (Category "seed") id

// Part 1
let getLowestLocation almanac =
    let seedToLocation = makeSeedToLocation almanac
    almanac.Seeds |> List.map seedToLocation |> List.min

getLowestLocation parsedSample
getLowestLocation parsedFile

// Part 2
let getLowestLocationRanged almanac =
    let seedToLocation = makeSeedToLocation almanac

    let seeds =
        almanac.Seeds
        |> List.chunkBySize 2
        |> List.map (fun chunks -> chunks[0], chunks[1])

    let rec findSmallest smallest current max =
        if current > max then
            smallest
        else
            findSmallest (min smallest (seedToLocation current)) (current + 1UL) max

    seeds
    |> List.toArray
    |> Array.Parallel.map (fun (seed, range) -> findSmallest UInt64.MaxValue seed (seed + range))
    |> Array.min

getLowestLocationRanged parsedSample