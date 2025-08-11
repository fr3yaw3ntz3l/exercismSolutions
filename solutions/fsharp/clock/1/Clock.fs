module Clock

let create hours minutes = 
    let totalMinutes = hours * 60 + minutes
    let normalizedMinutes = ((totalMinutes % 1440) + 1440) % 1440
    let hh = normalizedMinutes / 60
    let mm = normalizedMinutes % 60
    (hh, mm)

let add minutes clock = 
    create (fst clock) (snd clock + minutes)

let subtract minutes clock = 
    create (fst clock) (snd clock - minutes)

let display clock = 
    let mm = 
        if snd clock < 10 then 
            $"0{snd clock}"
        else $"{snd clock}"
    let hh = 
        if fst clock < 10 then 
            $"0{fst clock}"
        else $"{fst clock}"
    hh + ":" + mm