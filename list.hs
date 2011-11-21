-- Funktion length': alternative Definition von length
length' :: [a] -> Integer
length' list = sum (map (\x -> 1) list)

-- Funktion takeWhile': alternative Definition von takeWhile
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' praed (x:xs)
    | praed x   = x : takeWhile' praed xs
    | otherwise = []

-- Funktion splitAt: alternative Definition von splitAt
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n_elems list = (take n_elems list, drop n_elems list)
