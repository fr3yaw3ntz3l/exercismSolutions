module GuessingGame

let reply (guess: int): string =
    match guess with
    | 42 -> "Correct"
    | x when x = 41 || x = 43 -> "So close"
    | x when x < 41 -> "Too low"
    | x when x > 43 -> "Too high"
