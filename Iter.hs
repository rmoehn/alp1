-- stellt Iterationsfunktionen zur Verfügung
module Iter (
    iter,
    iter',
)
where

-- Funktion iter': Iterationsfunktion mit drittem Parameter
iter' :: Int -> (a -> a) -> a -> a
iter' n f x
    | n == 0 = x
    | n  > 0 = f (iter' (n - 1) f x)

-- Funktion iter: Iterationsfunktion ohne dritten Parameter
iter :: Int -> (a -> a) -> (a -> a)
iter 0 _ = donothing
    where
    -- Funktion donothing: gibt ihr Argument zurück
    donothing :: a -> a
    donothing x = x
iter 1 f = f
iter n f
    | n < 0     = error "keine Iterationen mit negativen Anzahlen!"
    | otherwise = f . (iter (n-1) f)
