-- Funktion ++: fügt zwei Listen zusammen
(+++) :: [a] -> [a] -> [a]
(+++) a b = foldr (:) b a

-- Funktion join: fügt eine Liste von Listen zu einer flachen Liste
-- unter Benutzung eines Verbindungselements zusammen
join :: [a] -> [[a]] -> [a]
join joiner listlist = foldl1 (\x y -> x ++ joiner ++ y) listlist

-- Funktion concat': fügt eine Liste von Listen zu einer flachen Liste
-- zusammen
concat' :: [[a]] -> [a]
concat' listlist = join [] listlist

-- Funktion split: trennt eine Liste an den Stellen, wo das angegebene Muster
-- vorkommt, auf und gibt eine Liste der Teillisten zurück
split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split muster list =
       [(take nxt_part_len list)]
    ++ split muster (drop (nxt_part_len + length muster) list)
    where nxt_part_len = partlength muster list

-- Funktion trennen: wie split, nur dass die leere Liste als Teilliste
-- angehängt wird, falls das Muster auch am Ende der ursprünglichen Liste
-- steht (wichtig, damit verbinden x . trennen x = id x)
trennen :: Eq a => [a] -> [a] -> [[a]]
trennen muster list =
    split muster list
    ++ if (drop (length list - length muster) list == muster) then
           [[]]
       else
           []

-- Funktion partlength: gibt die Länge der nächsten Teilliste zurück
partlength :: Eq a => [a] -> [a] -> Int
partlength _ [] = 0
partlength muster (x:xs)
    | nexttranch == muster = 0
    | otherwise            = 1 + partlength muster xs
    where nexttranch = take (length muster) (x:xs)
