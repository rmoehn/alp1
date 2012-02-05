import Num.Utils

-- Funktion kround: kaufmännisches Runden, d. h. ordentliches Runden auf zwei
-- Stellen nach dem Komma
kround :: RealFrac a => a -> a
kround x = (fromIntegral (round $ x * 100)) / 100

-- Funktion kshow: gibt eine Zahl kaufmännisch gerundet als String zurück
kshow :: RealFrac a => a -> String
kshow val
--    | mod integral_val 100 == 0 = show krounded_val ++ ".00"
    | mod integral_val  10 == 0 = show krounded_val ++ "0"
    | otherwise                 = show krounded_val
    where
    integral_val = floor $ kround (val * 100)
    krounded_val = kround val

-- Funktion kroshpad: gibt eine Zahl kaufmännisch gerundet als links mit
-- Leerzeichen auf die gewünschte Breite aufgefüllten String zurück
kroshpad :: RealFrac a => Int -> a -> String
kroshpad width val = (pad_string width ' ') (kshow val)


-- Funktion tilgungsplan: Berechnet einen Tilgungsplan für einen Kredit mit
-- festem Prozentsatz bei jährlichen unregelmäßigen Tilgungsraten. Die
-- Abrechnung erfolgt am 1. Januar des jeweiligen Jahres
tilgungsplan :: RealFrac a => Int -> a -> a -> [a] -> IO ()
tilgungsplan startjahr startschuld zins zahlungen
    = putStr ("Datum     Restschuld  Zahlung     Zinsen  Tilgung\n"
             ++ "1.1." ++ show startjahr ++ kroshpad 12 startschuld ++ "\n"
             ++ tilgungen (startjahr + 1) startschuld zins zahlungen)

-- Funktion tilgung: berechnet rekursiv die Tilgungen für die angegebenen
-- Zahlungen
tilgungen :: RealFrac a => Int -> a -> a -> [a] -> String
tilgungen _ _ _ [] = ""
tilgungen jahr schuld zins (z:zs)
    = "1.1." ++ show jahr ++ kroshpad 12 restschuld ++ kroshpad 12 zahlung
      ++ kroshpad 8 zinsen ++ kroshpad 9 tilgung ++ "\n"
      ++ tilgungen (jahr + 1) restschuld zins zs
    where
    restschuld = schuld - tilgung
    tilgung    = zahlung - zinsen
    zahlung    = z
    zinsen     = schuld * (zins / 100)

