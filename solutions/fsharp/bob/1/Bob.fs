module Bob

let rec isYelling str =
    let letters = str |> Seq.filter System.Char.IsLetter
    not (Seq.isEmpty letters) && Seq.forall System.Char.IsUpper letters

let response (input: string): string = 
    let trimmed = input.Trim()
    match trimmed with
    | "" -> "Fine. Be that way!"
    | x when x.EndsWith("?") && isYelling x -> "Calm down, I know what I'm doing!"
    | x when x.EndsWith("?") -> "Sure."
    | x when isYelling x -> "Whoa, chill out!"
    | _ -> "Whatever."