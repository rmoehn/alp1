-- Funktion gen_sublists: generiert eine Liste aller Teillisten einer Liste
-- (vgl. Teilmengen)
gen_sublists :: [a] -> [[a]]
gen_sublists []     = [[]]
gen_sublists (x:xs) = gen_some_sublists 1 (x:xs) ++ gen_sublists xs

-- Funktion gen_some_sublists: gibt eine Liste [[a1], [a1,a2], [a1,a2,a3],
-- ...] der Teillisten einer Liste [a1, a2, a3, ...] zurück
gen_some_sublists :: Int -> [a] -> [[a]]
gen_some_sublists _ []  = [[]]
gen_some_sublists cnt liste
    | cnt <  listlength = [take cnt liste] ++ gen_some_sublists (cnt+1) liste
    | cnt == listlength = [liste]
    where
    listlength = length liste

-- Funktion largest_sublistsum: gibt die größte Summe einer Teilfolge einer
-- Liste zurück
largest_sublistsum :: (Num a, Ord a) => [a] -> a
largest_sublistsum nums = maximum (map sum (gen_sublists nums))
