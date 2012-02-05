-- stellt Funktionen bereit, um mit Hash-ähnlichen Listen zu arbeiten:
-- [(Schlüssel, Wert)]
module Hash.TupListHash (
    value,
    exists,
)
where

type Hash a b = [(a, b)]

-- Funktion emptyhash: gibt einen leeren Hash zurück
emptyhash :: Hash a b
emptyhash = []


-- Funktion value: gibt den Wert zu einem bestimmten Schlüssel zurück
value :: Eq a => Hash a b -> a -> b
value [] _         = error "(Hash) Schlüssel existiert nicht."
value (x:xs) key
    | key == fst x = snd x
    | otherwise    = value xs key

-- Funktion exists: testet, ob der Schlüssel im gegebenen Pseudohash vorhanden
-- ist
exists :: Eq a => Hash a b -> a -> Bool
exists [] _        = False
exists (x:xs) key
    | key == fst x = True
    | otherwise    = exists xs key

-- Funktion ins: fügt ein Schlüssel-Wert-Paar in einen Hash ein
ins :: Eq a => Hash a b -> a -> b -> Hash a b
ins hash k v = (k, v) : (delete hash k)

-- Funktion delete: entfernt alle Schlüssel-Wert-Paare mit dem angegebenen
-- Schlüssel aus einem Hash
delete :: Eq a => Hash a b -> a -> Hash a b
delete hash key = [(k, v) | (k, v) <- hash, k /= key]
