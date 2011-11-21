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
