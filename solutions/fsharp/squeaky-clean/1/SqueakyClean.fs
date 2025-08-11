module SqueakyClean

open System

let isGreekLowerCase c = 
    int c >= 0x03B1 && int c <= 0x03C9

let transform (c: char) : string =
    match c with
    | '-' -> $"_"
    | ' ' -> $""
    | c when Char.IsDigit c -> $""
    | c when isGreekLowerCase c -> $"?"
    | c when Char.IsLetter c && c = Char.ToUpper c -> $"-{Char.ToLower c}"
    | _ -> string c
    
let clean (identifier: string): string =
    identifier |> String.collect transform

