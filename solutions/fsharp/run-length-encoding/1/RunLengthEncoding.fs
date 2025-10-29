module RunLengthEncoding

let encode input =
    let rec aux lst count acc =
        match lst with
        | [] -> acc
        | [x] -> (count + 1, x) :: acc
        | x :: (y :: _ as rest) ->
            if x = y then
                aux rest (count + 1) acc
            else
                aux rest 0 ((count + 1, x) :: acc)
    input
    |> Seq.toList
    |> fun lst -> aux lst 0 []
    |> List.rev
    |> List.map (fun (n, c) -> 
        if n = 1 then string c 
        else string n + string c)
    |> String.concat ""


let decode (input: string) =
    let rec aux chars acc numStr =
        match chars with
        | [] -> acc
        | c :: rest ->
            if System.Char.IsDigit c then
                aux rest acc (numStr + string c)
            else
                let count = if numStr = "" then 1 else int numStr
                aux rest (acc + String.replicate count (string c)) ""
    aux (Seq.toList input) "" ""
