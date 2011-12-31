-- Typklasse für Boole'sche Funktionen
class BoolFunc a where
    stellen :: a -> Int
    chart   :: a -> String

instance BoolFunc Bool where
    -- Anker
    stellen bool = 0

-- Wrapper, damit Standardhaskell das mitmacht
class Boolean b where
    true :: b
instance Boolean Bool where
    true = True

instance (BoolFunc a, Boolean b) => BoolFunc (b -> a) where
    -- berechnet Stellenzahl für beliebige Boole'sche Funktion
    stellen func = (stellen (func true)) + 1

--testfunc :: (BoolFunc a, Boolean b) => b -> a
testfunc = \x y z -> x && y || z

applicator :: BoolFunc a => (Bool -> a) -> [Bool] -> (Bool -> a)
applicator func (b:bs) = applicator (func b) bs
