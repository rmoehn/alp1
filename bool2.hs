{-# OPTIONS_GHC -XFlexibleInstances #-}

class BoolFunk a where
    stellenzahl :: a -> Int

instance BoolFunk Bool where
    stellenzahl bool = 0

instance (BoolFunk a) => BoolFunk (Bool -> a) where
    stellenzahl func = (stellenzahl (func True)) + 1

testfunc :: Bool -> Bool -> Bool -> Bool
testfunc = (\x y z -> x && y || z)

--apply func boollist = apply' (stellenzahl func) func boollist

apply' :: BoolFunk bf => Int -> (Bool -> bf) -> [Bool] -> (Bool -> bf)
apply' n func (b:bs)
    | n == 0    = func
    | otherwise = apply' (n-1) (func b) bs
