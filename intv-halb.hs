-- Funktion nullstelle: findet die Nullstelle einer Funktion f in angegebenem
-- Intervall [a; b] mit dem Intervallhalbierungsverfahren
nullstelle :: (Floating a, Ord a) => (a -> a) -> a -> a -> a -> a
nullstelle f a b epsi
    -- Abbruchbedingungen
    | (b - a) < epsi = mid_arg
    | mid_val == 0   = mid_arg
    -- Ausschließen von Quatsch
    | a > b
        = error "a muss kleiner sein als b."
    | signum fa == signum fb
        = error "Vorzeichen der Intervallgrenzenwerte müssen verschieden sein."
    -- Rekursion
    | signum mid_val == signum fa = nullstelle f mid_arg b epsi
    | otherwise                   = nullstelle f a mid_arg epsi
    where
    fa = f a
    fb = f b
    mid_arg = (a + b) / 2
    mid_val = f mid_arg
