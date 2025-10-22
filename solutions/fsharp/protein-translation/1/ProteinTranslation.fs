module ProteinTranslation

let proteins rna = 
    let lst = Seq.toList rna 
    let rec aux lst acc =
        match lst with
        | [] -> acc
        | x :: y :: z :: rest ->
            match x, y, z with
            | ('A', 'U', 'G') -> aux rest (["Methionine"] @ acc)
            | ('U', 'U', 'U') | ('U', 'U', 'C') -> aux rest (["Phenylalanine"] @ acc)
            | ('U', 'U', 'A') | ('U', 'U', 'G') -> aux rest (["Leucine"] @ acc)
            | ('U', 'C', 'U') | ('U', 'C', 'C') | ('U', 'C', 'A') | ('U', 'C', 'G') -> aux rest (["Serine"] @ acc)
            | ('U', 'A', 'U') | ('U', 'A', 'C') -> aux rest (["Tyrosine"] @ acc)
            | ('U', 'G', 'U') | ('U', 'G', 'C') -> aux rest (["Cysteine"] @ acc)
            | ('U', 'G', 'G') -> aux rest (["Tryptophan"] @ acc)
            | ('U', 'A', 'A') | ('U', 'A', 'G') | ('U', 'G', 'A') -> acc
    List.rev (aux lst [])

