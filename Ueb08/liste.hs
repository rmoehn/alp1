-- Funktion verschmelze: vereinigt zwei aufsteigend sortierte Listen zu einer
-- aufsteigend sortierten Gesamtliste
verschmelze :: Ord a => [a] -> [a] -> [a]
verschmelze list_a [] = list_a
verschmelze [] list_b = list_b
verschmelze (a:as) (b:bs)
    | a < b     = a : verschmelze as (b:bs)
    | otherwise = b : verschmelze (a:as) bs
