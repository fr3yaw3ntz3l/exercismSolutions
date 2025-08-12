module Allergies

open System

type Allergen =
    | Eggs
    | Peanuts
    | Shellfish
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats

let getAllergenCode allergen =
    match allergen with
    | Eggs -> 1
    | Peanuts -> 2
    | Shellfish -> 4
    | Strawberries -> 8
    | Tomatoes -> 16
    | Chocolate -> 32
    | Pollen -> 64
    | Cats -> 128

let allergicTo codedAllergies allergen = 
    let allergenCode = getAllergenCode allergen
    (codedAllergies &&& allergenCode) = allergenCode

let list codedAllergies = 
    let allergies = [Eggs; Peanuts; Shellfish; Strawberries; Tomatoes; Chocolate; Pollen; Cats]
    let rec aux codedAl lst =
        match lst with
        | [] -> []
        | x :: xs ->
            if allergicTo codedAl x then
                x :: aux (codedAl - getAllergenCode x) xs
            else aux codedAl xs
    aux codedAllergies allergies