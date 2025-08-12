module ResistorColorTrio

let getNum color = 
    match color with
    | "black" -> 0
    | "brown" -> 1
    | "red" -> 2
    | "orange" -> 3
    | "yellow" -> 4
    | "green" -> 5
    | "blue" -> 6
    | "violet" -> 7
    | "grey" -> 8
    | "white" -> 9

let getZeros color =
    let number = getNum color
    let rec aux num =
        if num < 1 then ""
        else "0" + aux (num - 1)
    aux number

let rec label colors =
    match colors with
    | [first; second; third] ->
        let value = string (getNum first) + string (getNum second) + getZeros third
        let length = value.Length
        
        if length >= 10 then
            value.Substring(0, length - 9) + " gigaohms"
        elif length >= 7 then
            value.Substring(0, length - 6) + " megaohms"
        elif length >= 4 then 
            value.Substring(0, length - 3) + " kiloohms"
        elif value.Substring(0, 1) = "0" then
            value.Substring(1) + " ohms"
        else
            value + " ohms"
    | c1 :: c2 :: c3 :: rest -> label [c1; c2; c3]