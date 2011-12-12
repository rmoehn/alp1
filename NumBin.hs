-- Typesynonyme als Instanznamen erlauben
{-# OPTIONS_GHC -XTypeSynonymInstances #-}

-- Modul zur Bereitstellung binärer Zahlen
module NumBin (
    Bin,
    pad_bin,
    test,
    tests
) where

type Bin = [Int]

-- Binärzahlen als Instanz von Enum. -- Binärzahlen werden als Liste von 0 und
-- 1 im Big-Endian-Format aufgefasst.
instance Enum Bin where
    fromEnum [] = 0
    fromEnum (x:xs) = x * 2^(length xs) + fromEnum xs
    toEnum y = reverse (deztobin y)
        where
        -- wandelt Dezimalzahl in Little-Endian-Binärzahl um
        deztobin :: Int -> Bin
        deztobin 0 = []
        deztobin x = mod x 2 : deztobin (div x 2)

-- Funktion pad_bin: bringt eine Binärzahl auf die vorgegebene Länge
pad_bin :: Int -> Bin -> Bin
pad_bin lnth binnum = replicate (lnth - length binnum) 0 ++ binnum
