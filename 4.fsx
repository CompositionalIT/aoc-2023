#load "common.fsx"
open System

type Card = {
    Id: int
    Winning: int Set
    Ticket: int Set
} with

    member this.WinningNumbers = (Set.intersect this.Ticket this.Winning).Count

let parse line =
    match line with
    | Split ':' [ Split ' ' [ _; Int card ]; Split '|' [ Split ' ' winning; Split ' ' ticket ] ] ->
        let winning = winning |> List.map int |> Set
        let ticket = ticket |> List.map int |> Set

        {
            Id = card
            Winning = winning
            Ticket = ticket
        }
    | _ -> failwith $"bad input '{line}'"

let ticketScore (card: Card) =
    Math.Pow(2, float (card.WinningNumbers - 1)) |> int

// Part 1
let sample =
    """
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"""

let parsedSample = sample.ByNewLine() |> Array.map parse
let parsedFile = Files[4] |> Array.map parse
parsedSample |> Array.sumBy ticketScore
parsedFile |> Array.sumBy ticketScore

// Part 2
let totalScratchcards cards =
    let state = cards |> Array.map (fun c -> {| Id = c.Id; Count = 1 |})

    (state, cards)
    ||> Array.fold (fun state (card: Card) ->
        let toUpdate =
            if card.WinningNumbers = 0 then
                None
            else
                Some(card.Id + 1, card.Id + card.WinningNumbers)

        let currentCardCount = state |> Array.find (fun row -> row.Id = card.Id) |> _.Count

        let state =
            state
            |> Array.map (fun row ->
                match toUpdate with
                | None -> row
                | Some(min, max) ->
                    if row.Id >= min && row.Id <= max then
                        {|
                            row with
                                Count = row.Count + currentCardCount
                        |}
                    else
                        row)

        state)
    |> Array.sumBy (fun row -> row.Count)

totalScratchcards parsedSample
totalScratchcards parsedFile