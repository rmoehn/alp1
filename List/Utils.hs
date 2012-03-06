module List.Utils
where

-- Funktion permutationen: gibt alle n! Permutationen der Zahlen von 1 bis n
-- aus
permutationen :: Int -> [[Int]]
permutationen n = permutationen' 0 [1..n]
    where
    permutationen' :: Int -> [Int] -> [[Int]]
    permutationen' _ [x] = [[x]]
    permutationen' index list
        | index < (length list)
            = (map ((:) indexelem) $ permutationen' 0 restliste)
              ++ permutationen' (index + 1) list
        | otherwise = []
        where
        indexelem = list !! index
        restliste = take index list ++ drop (index + 1) list

-- Funktion inc_array_ind: inkrementiert den Wert bei index in einem Array um
-- 1
inc_array_ind :: Num a => Int -> [a] -> [a]
inc_array_ind index ary = firstpart ++ [val + 1] ++ secpart
    where
    val       = ary !! index
    firstpart = take index ary
    secpart   = drop (index + 1) ary

-- Funktion count_occ: Berechnet für eine Eingabeliste mit Zahlen 0 .. maxval,
-- wie oft die Zahl darin vorkommt. Ausgabe ist eine Liste, bei deren Indizes
-- 0 ..  n die Anzahlen stehen
count_occ :: [Int] -> Int -> [Int]
count_occ inpt_list maxval = count_occ' inpt_list (replicate maxval 0)
    where
    -- man braucht eine Anfangsliste, wo alle Anzahlen auf 0 stehen
    count_occ' []     occ_rec = occ_rec
    count_occ' (x:xs) occ_rec = inc_array_ind x (count_occ' xs occ_rec)
