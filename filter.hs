-- Funktion rec_filter: rekursive Implementation von filter
rec_filter :: (a -> Bool) -> [a] -> [a]
rec_filter _ [] = []
rec_filter praed (x:xs)
    | praed x   = x : filter praed xs
    | otherwise = filter praed xs

-- Funktion zf_filter: Implementation von filter als List-Comprehension
zf_filter :: (a -> Bool) -> [a] -> [a]
zf_filter praed elems = [elem | elem <- elems, praed elem]

-- Funktion rem_spaces: entfernt alle Leerzeichen aus einer Zeichenkette
rem_spaces :: String -> String
rem_spaces = filter (/=' ')

