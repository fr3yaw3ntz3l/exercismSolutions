module Pangram

let isPangram (input: string): bool = 
    input 
    |> Seq.filter System.Char.IsLetter
    |> Seq.map System.Char.ToLower
    |> Set.ofSeq
    |> Set.count = 26