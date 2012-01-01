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

teststring :: String
teststring = "27D5D-2UD7UUD44D8UD5D7UD9UUU"

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

-- Funktion uncodify: macht aus einem String im codify-Format wieder einen
-- Baum' a
uncodify :: Read a => String -> Baum' a
uncodify code
    | d_cnt > 0 = Knoten k_val (uncodify firstbranch) (uncodify secbranch)
    | otherwise = Blatt b_val
    where
    -- den Wert des Knotens/Blatts enthaltender String
    k_str = takeWhile (\ c -> (updownelse c) /=  1) code
    b_str = takeWhile (\ c -> (updownelse c) /= -1) code
    -- String in den geforderten Typ umwandeln (Geht nicht immer.)
    k_val = read k_str
    b_val = read b_str
    -- was nach dem Knotenstring kommt ohne initiales 'D'
    after_knot  = drop ((length k_str) + 1) code
    -- Code für linken Ast
    firstbranch = next_coded_branch 1 after_knot
    -- Code für rechten Ast
    secbranch   = drop ((length firstbranch) + 1) after_knot
    -- Wenn kein 'D' im String ist, ist es ein Blatt.
    d_cnt = sum (map (\c -> if c == 'D' then 1 else 0) code)

-- Funktion updownelse: gibt -1, 1 oder 0 zurück, je nach dem, ob das
-- eingegebene Zeichen U, D oder ein anderes ist
updownelse :: Char -> Int
updownelse 'U' = -1
updownelse 'D' = 1
updownelse _   = 0

-- Funktion next_coded_branch: gibt den Teil eines Strings im codify-Format,
-- beginnend nach dem initialen 'D', zurück, der einen Zweig repräsentiert
--
-- Funktionsweise: String durchgehen, bei D 1 hochzählen, bei U 1
-- subtrahieren, zurückkehren, wenn wieder bei 0
--   5D-2UD7UU
-- 1  2  12 10
next_coded_branch :: Int -> String -> String
next_coded_branch 0 _  = []
next_coded_branch _ [] = []
next_coded_branch a (x:xs) = x : next_coded_branch (a + (updownelse  x)) xs

