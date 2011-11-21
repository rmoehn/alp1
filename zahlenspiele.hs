import Iter

-- Achtung: Die folgenden Funktionen funktionieren (!) nur mit natürlichen
-- Zahlen.

-- Funktion potenz: Potenzieren unter Verwendung iterierter Multiplikation
potenz :: Int -> Int -> Int
potenz a b = iter b (mal a) 1 -- = a^b

-- Funktion turm: Turmfunktion unter Verwendung iterierter Potenzierung
turm :: Int -> Int -> Int -- hier kein Float, da die Zahl hoch sich selbst
                          -- genommen wird
turm a b = iter (b-1) (potenz a) a -- = a ↑ b

-- Funktion mal: Multiplikation unter Verwendung iterierter Addition
mal :: Int -> Int -> Int
mal a b = iter b (plus a) 0 -- = a * b

-- Funktion plus: Addition unter Verwendung iterierter Nachfolgeoperation
plus :: Int -> Int -> Int
plus a b
    | a < 0     = error "unnatürliche Zahl im Argument"
    | otherwise = iter b next a -- = a + b

-- Funktion next: Nachfolgerfunktion
next :: Int -> Int
next a = a + 1

-- Funktion entdecke: findet das Argument n aus einer Funktion (iter n) heraus
entdecke :: (a -> a) -> a
entdecke iterfunc = iterfunc (+1) 0

-- Funktion proditer: Funktion von zwei Funktionen (iter a) und (iter b),
-- sodass proditer (iter a) (iter b) == iter (a * b)
proditer :: (a -> a) -> (a -> a) -> (a -> a)
proditer itfunc1 itfunc2 = itfunc1 . itfunc2

-- Funktion sumiter: Funktion von zwei Funktionen (iter a) und (iter b),
-- sodass sumiter (iter a) (iter b) == iter (a + b)
sumiter :: (a -> a) -> (a -> a) -> (a -> a) -> (a -> a)
sumiter itfunc1 itfunc2 f = (itfunc1 f) . (itfunc2 f)
