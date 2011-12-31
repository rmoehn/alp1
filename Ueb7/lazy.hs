-- Funktion mult: träge Multiplikation zweier Zahlen
mult :: Num a => a -> a -> a
mult 0 _ = 0
mult a b = a * b
