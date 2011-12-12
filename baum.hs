import Num_Utils

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

-- Funktion prettyprint: gibt einen Baum vom Typ Baum' `Num` in schöner Form
-- aus
prettyprint :: Num a => Baum' a -> IO ()
prettyprint baum = putStr (prprebenen lnlength 0 (ebenen baum))
    where lnlength = 2^(hoehe baum) * (1 + maxelemlength baum) - 1

-- Funktion prprebenen: tut die eigentliche Arbeit von prettyprint, braucht
-- aber eine Liste der Ebenen als Eingabe
prprebenen :: Num a => Int -> Int -> [[Maybe a]] -> String
prprebenen lnlength actdepth (eb:ebs)
    | ebs == [] = prfields fieldlength (ebtostrings eb)
    | otherwise = concat (
          map (prfields fieldlength) [
              ebtostrings eb,
              replicate fieldcnt "|",
              replicate fieldcnt (prverteiler lnlength (actdepth + 1))
          ]
      )
      ++ prfields (div fieldlength 2) (replicate (fieldcnt * 2) "|")
      ++ prprebenen lnlength (actdepth + 1) ebs
    where
    fieldlength = 1 + div lnlength (length eb)
    fieldcnt    = 2^actdepth

-- Funktion ebtostrings: nimmt eine Liste von Werten einer Ebene (Maybe a) und
-- macht sie zu einer Liste von Strings
ebtostrings :: Show a => [Maybe a] -> [String]
ebtostrings = map (maybe "" show)

-- Funktion prfields: Gibt beliebige Felder fein zentriert und als Zeile aus
prfields :: Int -> [String] -> String
prfields fieldlength [f]    = center_string (fieldlength - 1) ' ' f ++ "\n"
prfields fieldlength (f:fs) = center_string fieldlength ' ' f
                              ++ prfields fieldlength fs

-- Funktion prverteiler: Gibt den Verteiler (das vertikale Gebilde ähnlich
-- +--+--+) für die aktuelle Tiefe zurück
prverteiler :: Int -> Int -> String
prverteiler lnlength actdepth
    = "+"
      ++ center_string (div lnlength (2^actdepth)) '-' "+"
      ++ "+"

-- Funktion prvline: gibt eine Liste der Werte einer Ebene als String zurück
prvline :: Show a => Int -> [Maybe a] -> String
prvline fieldlength [v] = pad_string (fieldlength - 1) ' ' (maybe "" show v)
prvline fieldlength (v:vs)
    = pad_string fieldlength ' ' (maybe "" show v)
      ++ prvline fieldlength vs

-- Funktion maxelemlength: findet die Länge des längsten Elements in einem
-- Baum'
maxelemlength :: Num a => Baum' a -> Int
maxelemlength baum = longerelem 0 baum

-- Funktion longerelem: durchsucht einen Baum nach einem Element, das länger
-- ist als die angegebene Länge
longerelem :: Num a => Int -> Baum' a -> Int
longerelem l (Knoten v zl zr)
    = max (longerelem lvlonger zl) (longerelem lvlonger zr)
    where lvlonger = max l (length (show v))
longerelem l (Blatt v) = max l (length (show v))
longerelem l (Leer)    = l

-- Funktion hoehe: gibt die Höhe eine Baumes zurück (maximale Tiefe eines
-- Elements)
hoehe :: Baum' a -> Int
hoehe (Knoten _ zl zr) = 1 + max (hoehe zl) (hoehe zr)
hoehe (Blatt _) = 0
hoehe (Leer)    = 0

-- Funktion ebenen: gibt die Werte in den einzelnen Ebenen des Baumes als
-- Liste von Listen zurück
ebenen :: Baum' a -> [[Maybe a]]
ebenen baum = [ebene i baum | i <- [0..(hoehe baum)]]

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
