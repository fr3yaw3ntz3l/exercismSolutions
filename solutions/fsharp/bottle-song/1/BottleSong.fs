module BottleSong

let numWord n =
     match n with
     | 0 -> "Zero"
     | 1 -> "One"
     | 2 -> "Two"
     | 3 -> "Three"
     | 4 -> "Four"
     | 5 -> "Five"
     | 6 -> "Six"
     | 7 -> "Seven"
     | 8 -> "Eight"
     | 9 -> "Nine"
     | 10 -> "Ten"

let rec recite startBottles takeDown =
    match startBottles, takeDown with
    | _, 0 -> []
    | x, 1 -> 
        if x = 1 then
            ["One green bottle hanging on the wall,"; 
             "One green bottle hanging on the wall,"; 
             "And if one green bottle should accidentally fall,"; 
             "There'll be no green bottles hanging on the wall."]
        elif x = 2 then
            ["Two green bottles hanging on the wall,"; 
             "Two green bottles hanging on the wall,"; 
             "And if one green bottle should accidentally fall,"; 
             "There'll be one green bottle hanging on the wall."]
        else
            [$"{numWord x} green bottles hanging on the wall,";
             $"{numWord x} green bottles hanging on the wall,";
             $"And if one green bottle should accidentally fall,";
             $"There'll be {(numWord (x - 1)).ToLower()} green bottles hanging on the wall."]
    | x, y -> 
        let bottleWord = if x - 1 = 1 then "bottle" else "bottles"
        $"{numWord x} green bottles hanging on the wall," :: 
        $"{numWord x} green bottles hanging on the wall," :: 
        $"And if one green bottle should accidentally fall," :: 
        $"There'll be {(numWord (x - 1)).ToLower()} green {bottleWord} hanging on the wall." :: 
        "" ::  
        recite (x - 1) (y - 1)