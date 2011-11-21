-- Funktion biggerThanAVG3: gibt an, wie viele von drei Zahlen größer als ihr
-- Durchschnitt sind
biggerThanAVG3 :: Integer -> Integer -> Integer -> Integer
biggerThanAVG3 x y z =
    sum (
        map (\k -> if fromIntegral k > avg3 x y z then 1 else 0) [x, y, z]
    )
    where
    avg3 :: Integer -> Integer -> Integer -> Double
    avg3 a b c = fromIntegral (a+b+c) / 3
