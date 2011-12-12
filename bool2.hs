{-# OPTIONS_GHC -XFlexibleInstances #-}
{-# OPTIONS_GHC -XTypeSynonymInstances #-}

import Num_Bin

-- Typklasse für Boole'sche Funktionen verlangt...
class BoolFunk a where
    -- Stellenzahl der Funktion
    stellenzahl  :: a -> Int
    -- Variante der Funktion, die auf eine Liste von Parameter angewandt
    -- werden kann
    multiuncurry :: a -> ([Bool] -> Bool)

-- Bool-Werte als Instanzen von BoolFunk
instance BoolFunk Bool where
    stellenzahl  bool    = 0
    multiuncurry bool [] = bool

-- beliebige Boole'sche Funktion als Instanz von BoolFunk
instance (BoolFunk a) => BoolFunk (Bool -> a) where
    -- Funktion Argument für Argument auswerten bis Bool übrigbleibt
    stellenzahl func = (stellenzahl (func True)) + 1
    -- Argumente aus der Liste schrittweise zuführen
    multiuncurry f (b:bs) = multiuncurry (f b) bs

-- Funktion bintobool: wandelt eine Binärzahl in eine Liste von Bools um
bintobool :: Bin -> [Bool]
bintobool binnum = map (==1) binnum

-- Funktion showftlist: gibt eine Liste von Bool als String aus
showftlist :: [Bool] -> String
showftlist [] = ""
showftlist (b:bs) = (if b == True then "T " else "F ") ++ showftlist bs

-- Funktion wahrheitstabelle: gibt für eine beliebigstellige Boole'sche
-- Funktion die Wahrheitstabelle mit allen Belegungen aus
wahrheitstabelle :: BoolFunk a => a -> IO ()
wahrheitstabelle func = putStr (unlines (
                            map (
                                (belzeile mucfunc)
                                . bintobool
                                . (pad_bin stellen)
                            ) [startbin .. endbin]
                        ))
    where
    stellen  = stellenzahl func
    startbin = replicate stellen 0
    endbin   = replicate stellen 1
    mucfunc  = multiuncurry func


-- Funktion belzeile: Gibt eine Zeile einer Wahrheitstabelle für eine konkrete
-- Belegung aus. mucfunc ist die multiungecurriete (!) Boole'sche Funktion
belzeile :: ([Bool] -> Bool) -> [Bool] -> String
belzeile mucfunc bel = showftlist bel ++ "| "
                       ++ showftlist [mucfunc bel]

testfunc :: Bool -> Bool -> Bool -> Bool
testfunc = (\x y z -> x && y || z)
