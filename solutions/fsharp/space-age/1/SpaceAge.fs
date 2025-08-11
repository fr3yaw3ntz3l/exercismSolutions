module SpaceAge

type Planet =
    | Mercury 
    | Venus 
    | Earth 
    | Mars 
    | Jupiter
    | Saturn 
    | Uranus 
    | Neptune

let planetYearInSeconds x = x * 31557600.0

let age (planet: Planet) (seconds: int64): float = 
    match planet, seconds with
    | Mercury, s -> float s / planetYearInSeconds 0.2408467
    | Venus, s -> float s / planetYearInSeconds 0.61519726
    | Earth, s -> float s / planetYearInSeconds 1.0
    | Mars, s -> float s / planetYearInSeconds 1.8808158
    | Jupiter, s -> float s / planetYearInSeconds 11.862615
    | Saturn, s -> float s / planetYearInSeconds 29.447498
    | Uranus, s -> float s / planetYearInSeconds 84.016846
    | Neptune, s -> float s / planetYearInSeconds 164.79132