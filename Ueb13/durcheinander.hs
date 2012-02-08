import Num.Bin

-- Funktion list_from_mask: Nimmt von zwei Listen immer den ersten Wert weg
-- und fügt den aus der ersten der Ergebnisliste hinzu, wenn der Wert aus der
-- zweiten Liste gleich dem dritten Argument ist
list_from_mask :: [a] -> Bin -> Int -> [a]
list_from_mask [] [] _ = []
list_from_mask (l:ls) (b:bs) cmp
    | b == cmp  = l : list_from_mask ls bs cmp
    | otherwise = list_from_mask ls bs cmp

-- Funktion listpair_from_bin: Generiert aus einer Liste und einem Bitvektor
-- ein Tupel von Teillisten der Eingabeliste. -- Die Elemente, für die 0 im
-- Bitvektor steht, kommen in die linke Teilliste, die Elemente mit 1 in die
-- rechte.
listpair_from_bin :: [a] -> Bin -> ([a], [a])
listpair_from_bin inp_list mask
    = (list_from_mask inp_list mask 0, list_from_mask inp_list mask 1)

-- Funktion zerlegungen: generiert alle Zerlegungen einer Liste in  zwei
-- Teillisten
zerlegungen :: [a] -> [([a], [a])]
zerlegungen inplist = map (listpair_from_bin inplist) bitmasks
    where
    bitmasks = map (pad_bin inplistlength) [zeroes .. oneses]
    zeroes   = replicate inplistlength 0
    oneses   = replicate inplistlength 1
    inplistlength = length inplist

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
