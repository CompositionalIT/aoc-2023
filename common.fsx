[<AutoOpen>]
module Common

open System
open type System.Environment

type String with

    member this.ByNewLine() =
        this.Split([| NewLine; "\n" |], StringSplitOptions.RemoveEmptyEntries)

    /// This reusable function takes a multiline string and groups up based on whenever an empty line occurs.
    member this.GroupByEmptyLine() =
        this.Split([| NewLine + NewLine; "\n" + "\n" |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun group -> group.ByNewLine() |> Array.toList)
        |> Array.toList

type Files() =
    member _.Item
        with get file = System.IO.File.ReadAllLines $"data/{file}.txt"

/// Provides access to data files using an indexer e.g. Files.[1] gets the path to the Day One data file.
let Files = Files()