{-# OPTIONS_GHC -XFlexibleInstances #-}

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

testfunc :: Bool -> Bool -> Bool -> Bool
testfunc = (\x y z -> x && y || z)

