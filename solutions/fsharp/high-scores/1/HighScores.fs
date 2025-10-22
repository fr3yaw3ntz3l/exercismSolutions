module HighScores

let scores (values: int list): int list = values

let latest (values: int list): int = 
    values |> List.rev |> List.head

let personalBest (values: int list): int = 
    values |> List.sort |> List.rev |> List.head

let personalTopThree (values: int list): int list = 
    let sorted = values |> List.sort |> List.rev
    if sorted.Length > 3 then
        [sorted[0]; sorted[1]; sorted[2]]
    else sorted