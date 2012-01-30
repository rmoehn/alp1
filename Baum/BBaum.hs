module Baum.BBaum where

data BBaum a = Leer | Knoten a (BBaum a) (BBaum a)

bbaum1, bbaum2 :: BBaum Int
bbaum1 = Knoten 5 (Knoten 2 Leer (Knoten 3 Leer Leer))
              (Knoten 4 Leer Leer)
bbaum2 = Knoten 5 (Knoten 2 Leer (Knoten 3 Leer Leer))
              (Knoten 40 (Knoten 6 Leer Leer) Leer)

bbaum3 :: BBaum Char
bbaum3 = Knoten 'a' (
             Knoten 'd' (
                 Knoten 'y' (Leer Leer)
                 Leer
             )
             Knoten 'x' (
                 Knoten 'a' (Leer Leer)
                 Knoten 'y' (
                     Leer
                     Knoten 'd' (Leer Leer)
                 )
             )
         )
