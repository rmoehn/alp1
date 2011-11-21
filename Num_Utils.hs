module Num_Utils (
    pad_int,
    pad_string,
    teiler,
) where

-- Funktion pad_int: gibt eine Int-Wert als String aus, wobei links mit
-- dem angegebenen Zeichen aufgefüllt wird, bis die gewünschte Breite erreicht
-- ist
pad_int :: Int -> Int -> Char -> String
pad_int zahl breite zeichen = pad_string (show zahl) breite zeichen

---- Funktion pad_string: füllt einen String links mit dem angegebenen Zeichen
---- auf, bis er die gewünschte Breite erreicht hat
--pad_string :: String -> Int -> Char -> String
--pad_string string breite zeichen
--    | breite == 0 = ""
--    | otherwise = [zeichen] ++ (pad_string string (breite - length string - 1) zeichen)


-- Funktion pad_string: füllt einen String links mit dem angegebenen Zeichen
-- auf, bis er die gewünschte Breite erreicht hat
pad_string :: String -> Int -> Char -> String
pad_string string breite zeichen =
    replicate (breite - length string) zeichen ++ string

-- Funktion teiler: gibt eine Liste der echten Teiler einer Zahl zurück
teiler :: Integer -> [Integer]
teiler n = [x | x <- [1..(max_teiler n)], mod n x == 0]

-- Funktion max_teiler: gibt den größten möglichen echten Teiler einer Zahl
-- zurück
max_teiler :: Integer -> Integer
max_teiler n = floor ((fromIntegral n) / 2)
