data Baum' a = Blatt a
             | Knoten a (Baum' a) (Baum' a)
             | Leer
    deriving Show

testbaum'1 :: Baum' Int
testbaum'1 = (Knoten 27
               (Knoten 5
                   (Blatt (-2))
                   (Blatt 7)
               )
               (Knoten 44
                   (Blatt 8)
                   (Knoten 5
                       (Blatt 7)
                       (Blatt 9)
                   )
                )
            )

testbaum'2 :: Baum' Int
testbaum'2 = (Knoten 12
                 (Knoten 777
                     (Blatt 12)
                     (Knoten 22
                         (Blatt 16)
                         (Leer)
                     )
                 )
                 (Knoten 16
                     (Knoten 426
                         (Blatt 13)
                         (Blatt 6)
                     )
                     (Knoten (-12)
                         (Leer)
                         (Blatt 1)
                     )
                 )
             )

testbaum'3 :: Baum' Int
testbaum'3 = (Knoten 12
                 (Knoten 777
                     (Blatt 12)
                     (Knoten 22
                         (Blatt 16)
                         (Leer)
                     )
                 )
                 (Knoten 16
                     (Knoten 426
                         (Blatt 13)
                         (Blatt 6)
                     )
                     (Knoten (-12)
                         (Leer)
                         (Knoten 1
                             (Blatt 4)
                             (Blatt 90)
                         )
                     )
                 )
             )


-- Funktion codify: kodiert einen Baum vom Typ Baum' a
codify :: Show a => Baum' a -> String
codify (Knoten x zw1 zw2)
    = show x        -- Wert anzeigen
      ++ "D"        -- absteigen
      ++ codify zw1 -- linken Zweig kodieren
      ++ "UD"       -- aufsteigen, absteigen
      ++ codify zw2 -- rechten Zweig kodieren
      ++ "U"        -- aufsteigen
codify (Blatt x) = show x

---- Funktion prettyprint: gibt einen Baum vom Typ Baum' `Num` in schöner Form
---- aus
--prettyprint :: Num a => Baum' a -> IO ()
--prettyprint baum
--    where bhoehe = hoehe


-- Funktion maxelemlength: findet die Länge des längsten Elements in einem
-- Baum'
maxelem :: Num a => Baum' a -> Int
maxelem baum = longerelem 0 baum

-- Funktion longerelem: durchsucht einen Baum nach einem Element, das länger
-- ist als die angegebene Länge
longerelem :: Num a => Int -> Baum' a -> Int
longerelem l (Knoten v zl zr)
    = max (longerelem lvlonger zl) (longerelem lvlonger zr)
    where lvlonger = max l (length (show v))
longerelem l (Blatt v) = max l (length (show v))
longerelem l (Leer)    = l

-- Funktion hoehe: gibt die Höhe eine Baumes zurück
hoehe :: Baum' a -> Int
hoehe (Knoten _ zl zr) = 1 + max (hoehe zl) (hoehe zr)
hoehe (Blatt _) = 1
hoehe (Leer)    = 0

-- Funktion ebenen: gibt die Werte in den einzelnen Ebenen des Baumes als
-- Liste von Listen zurück
ebenen :: Baum' a -> [[Maybe a]]
ebenen baum = [ebene i baum | i <- [0..((hoehe baum) - 1)]]

-- Funktion ebene: gibt die Werte in der angegebenen Ebene des Baum' zurück
ebene :: Int -> Baum' a -> [Maybe a]
ebene eb (Knoten v zl zr)
    | eb == 0   = [Just v]
    | eb >  0   = ebene nxteb zl ++ ebene nxteb zr
    where
    nxteb = eb - 1
        -- Funktion steigt ab und gibt erst Werte zurück, wenn sie mit Ebene 0
        -- auf einen Knoten angewendet wird.
ebene eb (Leer) = replicate (2^eb) Nothing
        -- Leerstellen müssen korrekt gebildet werden
ebene eb (Blatt v)
    | eb == 0   = [Just v]
    | otherwise = replicate (2^eb) Nothing
