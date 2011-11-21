-- Funktion biggerThanAVG3': gibt eine Funktion zurück, die angibt, wie viele
-- von drei Zahlen größer als ihr Durchschnitt sind
biggerThanAVG3' :: (Integer -> Integer -> Integer -> Integer)
biggerThanAVG3' = \x y z ->
    sum (
        map (\k -> if fromIntegral k > (
                \a b c -> fromIntegral (a+b+c) /3
            ) x y z
            then 1 else 0
        ) [x, y, z]
    )

f x y = let n = 3 in take n (g y) ++ take n (g x)
    where
    g x = take n xys
        where
        xys = [x] ++ yxs
        yxs = [y] ++ xys
        n = 10

f_an = \x y -> [y,y,y,x,y,x]

f' = \g x y z -> x^3 - g (x + g (y - g z) + g (z^2))
g  = \x -> 2*x^2 + 10*x + 1
