module TisburyTreasureHunt

let getCoordinate (line: string * string): string = snd line

let convertCoordinate (coordinate: string): int * char = 
    (int (string coordinate[0]), coordinate[1])

let compareRecords (azarasData: string * string) (ruisData: string * (int * char) * string) : bool = 
    let azarasCoord = getCoordinate azarasData
    let (location, coordinate, quadrant) = ruisData
    if convertCoordinate azarasCoord = coordinate then true
    else false

let createRecord (azarasData: string * string) (ruisData: string * (int * char) * string) : (string * string * string * string) =
    let (treasure, azarasCoordinate) = azarasData
    let (location, ruisCoordinate, quadrant) = ruisData
    if compareRecords azarasData ruisData = true then 
        (azarasCoordinate, location, quadrant, treasure)
    else ("", "", "", "")
