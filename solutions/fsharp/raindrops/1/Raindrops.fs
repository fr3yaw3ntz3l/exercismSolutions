module Raindrops

let convert (number: int): string =
    let sounds = 
        [ if number % 3 = 0 then yield "Pling"
          if number % 5 = 0 then yield "Plang"
          if number % 7 = 0 then yield "Plong" ]

    if List.isEmpty sounds then string number
    else String.concat "" sounds