-- Funktion alleZerl: konstruiert alle Zerlegungen einer Liste in zwei
-- Teillisten
alleZerl :: Ord a => [a] -> [([a], [a])]
alleZerl [] = []
alleZerl list = alleZerl' 0 list
    where
    -- Wie alleZerl, nur mit einem notwendigen Parameter
    alleZerl' :: Ord a => Int -> [a] -> [([a], [a])]
    alleZerl' _ []  = []
    alleZerl' _ [x] = [([], [x]), ([x], [])]
    alleZerl' index list
        | index < (length list)
            = map (lpartlins indexelem) (alleZerl' 0 restliste)
              ++ alleZerl' (index + 1) list
        | otherwise = []
        where
        indexelem = list !! index
        restliste = take index list ++ drop (index + 1) list

        -- Funktion lpartlins: fügt ein Element in den Anfang der linken
        -- Teilliste eines Tupels ein, falls es kleiner oder gleich des ersten
        -- Elementes der Liste ist oder falls die Liste leer ist.
        lpartlins :: Ord a => a -> ([a], [a]) -> ([a], [a])
        lpartlins y ([], rpartl) = ([y], rpartl)
        lpartlins y ((x:xs), rpartl)
            | y <= x    = (y:x:xs, rpartl)
            | otherwise = ([], [])


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
