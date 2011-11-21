-- stellt Funktionen bereit, um mit Hash-ähnlichen Listen zu arbeiten:
-- [(Schlüssel, Wert)]
module Hash (
    value,
    exists,
)
where

-- Funktion value: gibt den Wert zu einem bestimmten Schlüssel zurück
value :: Eq a => [(a, b)] -> a -> b
value [] _         = undefined
value (x:xs) key
    | key == fst x = snd x
    | otherwise    = value xs key

-- Funktion exists: testet, ob der Schlüssel im gegebenen Pseudohash vorhanden
-- ist
exists :: Eq a => [(a, b)] -> a -> Bool
exists [] _        = False
exists (x:xs) key
    | key == fst x = True
    | otherwise    = exists xs key
