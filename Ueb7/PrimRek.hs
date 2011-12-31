{-# OPTIONS_GHC -XFlexibleInstances #-} -- HUGS needs the option "-98"

module PrimRek (N, nachf, zero, primRek) where

-- PRIMITIVE REKURSION

type N=Integer

nachf :: N->N
nachf x = x+1
zeroFunk :: N->N
zeroFunk _ = 0
zero :: N
zero = 0

class Funk a where
    stellenZahl:: a->Int -- stellenZahl (arity) is perhaps redundant
                         -- but useful for error checking
    multiuncurry :: a -> ([N] -> N)
-- (multiuncurry f) [x1,x2,...,xn] = f x1 x2 ... xn
    multicurry :: ([N] -> N) -> a
-- (multicurry g) x1 x2 ... xn = g [x1,x2,...,xn]

instance Funk Integer where
  stellenZahl _ = 0
  multiuncurry f [] = f
  multicurry g = g []

instance (Funk a) => Funk (N->a) where
    stellenZahl f = 1 + stellenZahl (f undefined)
-- (multiuncurry f) [x1,x2,...,xn] = f x1 x2 ... xn
    multiuncurry f (x:xs) = multiuncurry (f x) xs
-- (multicurry g) x1 x2 ... xn = g [x1,x2,...,xn]
    multicurry g x1 = multicurry g'
                        where g' xs = g (x1:xs)

primRek :: (Funk a, Funk b, Funk c) => a -> b -> c
primRek f g
   | stellenZahl g == stellenZahl f + 2
      = multicurry (primRekUncurry (multiuncurry f) (multiuncurry g))
   | otherwise = error "Stellenzahl passt nicht zusammen."

primRekUncurry f g = h where
      h (0:xs) = f xs
      h (n:xs) | n<0 = error "negative Parameter verboten."
               | n>0 = g (h':n':xs)
                                    where n' = n-1
                                          h' = h (n':xs)

-- BEISPIEL --------------------------------------

add:: N -> N -> N
add = primRek (id::N->N) g
   where g::N->N->N->N
         g h' n' b = nachf h'
         id::N->N
         id b = b


-- TESTING FUNCTIONS -----------------------------

s1 = multiuncurry (const::N->N->N)
 
f::N->N->N
f x = id

h::N->N->N->N
h x y z = 100*x+10*y+z

h'::[N]->N
h' [x, y, z] = 100*x+10*y+z


