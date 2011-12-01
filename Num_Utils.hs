module Num_Utils (
    pad_int,
    pad_string,
    teiler,
    avg,
) where

-- Funktion pad_int: gibt eine Int-Wert als String aus, wobei links mit
-- dem angegebenen Zeichen aufgefüllt wird, bis die gewünschte Breite erreicht
-- ist
pad_int :: Int -> Char -> Int -> String
pad_int breite zeichen zahl = pad_string breite zeichen (show zahl)

-- Funktion pad_string: füllt einen String links mit dem angegebenen Zeichen
-- auf, bis er die gewünschte Breite erreicht hat
pad_string :: Int -> Char -> String -> String
pad_string breite zeichen string =
    replicate (breite - length string) zeichen ++ string

-- Funktion center_string: zentriert einen String in einem Feld angegebener
-- Breite. Sollte genaue Zentrierung nicht möglich sein, wird rechtsgerückt
center_string :: Int -> Char -> String -> String
center_string breite zeichen string = replicate anffeldln zeichen
                                      ++ string
                                      ++ replicate endfeldln zeichen
    where
    stringln  = length string
    anffeldln = div (breite - stringln) 2
    endfeldln = breite - anffeldln - stringln


-- Funktion teiler: gibt eine Liste der echten Teiler einer Zahl zurück
teiler :: Integer -> [Integer]
teiler n = [x | x <- [1..(max_teiler n)], mod n x == 0]
    where
    -- Funktion max_teiler: gibt den größten möglichen echten Teiler einer Zahl
    -- zurück
    max_teiler :: Integer -> Integer
    max_teiler n = floor ((fromIntegral n) / 2)

-- Funktion avg: gibt den Durchschnitt einer Liste von Zahlen zurück
avg :: [Integer] -> Float
avg nums = fromIntegral (sum nums) / fromIntegral (length nums)
