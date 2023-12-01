[<AutoOpen>]
module Common

open System
open type System.Environment

/// This reusable function takes a multiline string and groups up based on whenever an empty line occurs.
let groupByLines (data: string) =
    data.Split([| NewLine + NewLine |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun group ->
        group.Split([| NewLine |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList)
    |> Array.toList

type Files() =
    member _.Item
        with get file = $"data/{file}.txt"

/// Provides access to data files using an indexer e.g. Files.[1] gets the path to the Day One data file.
let Files = Files()