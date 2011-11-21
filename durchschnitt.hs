-- Funktion avg: liefert den Durchschnitt dreier Variablen
avg :: Integer -> Integer -> Integer -> Float
avg x y z = (fromIntegral (x + y + z)) / 3

-- Funktion gt_avg_cnt: gibt die Anzahl der Parameter an, die größer als der
-- Durchschnitt der drei Parameter ist
gt_avg_cnt :: Integer -> Integer -> Integer -> Integer
gt_avg_cnt x y z
    | ((fromIntegral x > avg x y z) /= (fromIntegral y > avg x y z))
        /= (fromIntegral z > avg x y z) = 1
    | otherwise = 2
