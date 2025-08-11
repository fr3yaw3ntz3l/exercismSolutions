module LuciansLusciousLasagna

let expectedMinutesInOven = 40

let remainingMinutesInOven x = expectedMinutesInOven - x

let preparationTimeInMinutes x = x * 2

let elapsedTimeInMinutes x y = preparationTimeInMinutes x + y