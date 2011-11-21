module Finanz (
    zinsen,
    endwertn,
)
where

import Iter

-- Funktion zinsen: Berechnet die Zinsen einer Kapitalanlage bei
-- gegebenem Zinssatz
zinsen :: Float -> Float -> Float
zinsen kapital zinsfuss = kapital * zinsfuss / 100

-- Funktion endwert: Berechnet den Wert einer Kapitalanlage nach Ablauf
-- einer Zinsperiode
endwert :: Float -> Float -> Float
endwert kapital zinsfuss = kapital + (zinsen kapital zinsfuss)

-- Funktion endwert2: Berechnet den Wert einer Kapitalanlage nach Ablauf
-- zweier Zinsperioden
endwert2 :: Float -> Float -> Float
endwert2 kapital zinsfuss = endwert (endwert kapital zinsfuss) zinsfuss

-- Funktion endwertn: Berechnet den Wert einer Kapitalanlage nach Ablauf von n
-- Zinsperioden
endwertn :: Float -> Float -> Int -> Float
endwertn kapital zinsfuss n = priceround (iter n (endwert' zinsfuss) kapital)
    where
    -- Funktion endwert': wie endwert, nur mit umgekehrter Reihenfolge der
    -- Argumente
    endwert' :: Float -> Float -> Float
    endwert' zinsfuss kapital = endwert kapital zinsfuss

-- Funktion priceround: rundet auf zwei Stellen nach dem Komma
priceround :: Float -> Float
priceround preis = fromIntegral (round (preis * 100)) / 100
