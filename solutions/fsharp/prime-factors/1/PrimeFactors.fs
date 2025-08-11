module PrimeFactors

let factors (number: int64) : int list =
    let rec findFactors (number: int64) (divisor: int64) (acc: int64 list) : int64 list =
        if number < 2L then acc
        elif number % divisor = 0L then
            findFactors (number / divisor) divisor (divisor :: acc)
        else
            findFactors number (divisor + 1L) acc
    findFactors number 2L [] 
        |> List.rev
        |> List.map int