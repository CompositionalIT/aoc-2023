#load "common.fsx"
open System

// Part 1

type Game = {
    Id: int
    Draws: {| Color: string; Count: int |} list
}

let createGame (line: string) =
    let parts = line.Split(':', StringSplitOptions.TrimEntries)

    {
        Id = int (parts[0].Split(' ')[1])
        Draws = [
            for draw in parts[1].Split(';', StringSplitOptions.TrimEntries) do
                for ball in draw.Split(',', StringSplitOptions.TrimEntries) do
                    match ball.Split ' ' with
                    | [| count; color |] -> {|
                        Count = int count
                        Color = color.Trim()
                      |}
                    | _ -> failwith "Invalid draw"
        ]
    }

let getGameMaximums (game: Game) = {|
    game with
        Draws =
            game.Draws
            |> List.groupBy _.Color
            |> List.map (fun (color, draws) -> {|
                Color = color
                MaxCount = draws |> List.maxBy _.Count |> _.Count
            |})
|}

let getValidGameIds limits lines =
    lines
    |> Array.map (createGame >> getGameMaximums)
    |> Array.filter (fun game ->
        game.Draws
        |> List.forall (fun draw ->
            match limits |> Map.tryFind draw.Color with
            | Some limit -> draw.MaxCount <= limit
            | None -> false))
    |> Array.sumBy _.Id

let limits = Map [ "red", 12; "green", 13; "blue", 14 ]

let sample =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""

let lines = sample.ByNewLine()
lines |> getValidGameIds limits

Files[2] |> getValidGameIds limits

// Part 2
let getGamePowers lines =
    lines
    |> Array.sumBy (fun line ->
        line
        |> createGame
        |> getGameMaximums
        |> _.Draws
        |> List.map _.MaxCount
        |> List.reduce (*))

Files[2] |> getGamePowers