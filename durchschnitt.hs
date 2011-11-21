import Num_Utils

-- Funktion lt_avg_cnt: gibt die Anzahl der Werte einer Liste von Integern
-- zurück, die kleiner als der Durchschnitt der Werte sind
lt_avg_cnt :: [Integer] -> Integer
lt_avg_cnt nums = length' [num | num <- nums, (fromIntegral num) < avrge]
    where avrge = avg nums
