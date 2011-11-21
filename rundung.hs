-- Funktion zero: zur Demonstration von Rundungsfehlern bei Gleitkommarechnung
zero :: Float -> Float
zero x = (1/x) * x - 1 -- _sollte_ 0 sein

-- Funktion zerod: zur Demonstration von Rundungsfehlern bei
-- Gleitkommarechnung -- jetzt mit Double
zerod :: Double -> Double
zerod x = (1/x) * x - 1 -- _sollte_ 0 sein
