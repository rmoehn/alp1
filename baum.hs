data Baum' a = Blatt a
             | Knoten a (Baum' a) (Baum' a)
    deriving Show

testbaum :: Baum' Int
testbaum = (Knoten 27
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
