test :: [(Float,Float,Float)]
test = [(1,3,7),(0,0,0),(2,0,0),(1,5,5),(3,20,35)]
-- Funktion differenzen:  beschreibung
differenzen :: Integer -> Integer -> Integer -> Integer
differenzen a b c = foldr (-) a [b..c]

-- Funktion diffs:  beschreibung
diffs :: Float -> Float -> Float -> Float
diffs a b c = (-(c-b) + (-1 + (-1)^ex)/2)/2
              + (1 + (-1)^ex)/2*c
              - (-1)^ex * a
    where ex = floor (c-b)

-- Funktion differenzen:  beschreibung
differenzen' :: (Integer,Integer, Integer) -> Integer
differenzen' (a,b,c) = foldr (-) a [b..c]

-- Funktion diffs:  beschreibung
diffs' :: ( Float , Float , Float ) -> Float
diffs' ( a,b,c ) = (-(c-b) + (-1 + (-1)^ex)/2)/2
              + (1 + (-1)^ex)/2*c
              - (-1)^ex * a
    where ex = floor (c-b)
