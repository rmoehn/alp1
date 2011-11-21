import Num_Utils

-- Funktion hmin_to_min: wandelt eine in Stunden und Minuten gegebene Uhrzeit
-- in Minuten um
hmin_to_min :: (Int, Int) -> Int
hmin_to_min (h, min) = 60 * h + min

-- Funktion min_to_hmin: wandelt eine in Minuten gegebene Uhrzeit in Stunden
-- und Minuten um
min_to_hmin :: Int -> (Int, Int)
min_to_hmin min = (div min 60, abs (mod min 60))

-- Funktion uhrz_diff: berechnet die Differenz zwischen zwei Uhrzeiten in
-- Stunden und Minuten
uhrz_diff :: (Int, Int) -> (Int, Int) -> (Int, Int)
uhrz_diff (h1, min1) (h2, min2) =
    min_to_hmin ((hmin_to_min (h2, min2)) - (hmin_to_min (h1, min1)))

-- Funktion uhrz_24_to_12: wandelt eine Uhrzeit vom 24-Stunden- in das
-- 12-Stunden-Format (String) um
uhrz_24_to_12 :: (Int, Int) -> String
uhrz_24_to_12 (h, min)
    | (h `notElem` [0 .. 23]) || (min `notElem` [0 .. 60]) = "ungültig"
    | otherwise = show (hod_24_to_12 h) ++ ":" ++ pad_int min 2 '0'
                  ++ " " ++ part_of_day (h, min)

-- Funktion part_of_day: gibt den String "p. m.", "a. m.", "noon" oder
-- "midnight" aus, je nach dem, welche Zeit angegeben wurde
part_of_day :: (Int, Int) -> String
part_of_day (h, min)
    | (h == 12) && (min == 0) = "noon"
    | (h ==  0) && (min == 0) = "midnight"
    | (h <  12)               = "a. m."
    | (h <  24)               = "p. m."

-- Funktion hod_24_to_12: wandelt eine Stunde des 24-Stunden-Tages in eine
-- Stunde des 12-Stunden-Tages um
hod_24_to_12 :: Int -> Int
hod_24_to_12 h
    | h `elem` [0, 12, 24] = 12
    | otherwise = mod h 12

