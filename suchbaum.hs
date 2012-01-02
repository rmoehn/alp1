data Suchbaum a b
    = Leer
      | Knoten a b (Suchbaum a b) (Suchbaum a b)
    deriving Show

-- Funktion finde: Sucht einen Schlüssel in einem Suchbaum und gibt dessen
-- Wert zurück
finde :: Ord a => a -> Suchbaum a b -> Maybe b
finde _ Leer = Nothing
finde key (Knoten s w z_kl z_gr)
    | key < s   = finde key z_kl
    | key > s   = finde key z_gr
    | otherwise = Just w

-- Funktion einf: fügt ein Schlüssel-Wert-Paar in einen Suchbaum ein
einf :: Ord a => a -> b -> Suchbaum a b -> Suchbaum a b
einf s w Leer = Knoten s w Leer Leer
einf s w (Knoten k_s k_w z_kl z_gr)
    | s < k_s  = Knoten k_s k_w (einf s w z_kl) z_gr
    | s > k_s  = Knoten k_s k_w z_kl (einf s w z_gr)
    | s == k_s = Knoten k_s k_w z_kl z_gr

-- Funktion is_suchbaum: überprüft, ob ein Suchbaum richtig sortiert ist
is_suchbaum :: Ord a => Suchbaum a b -> Bool
is_suchbaum Leer = True
is_suchbaum (Knoten s _ z_kl z_gr)
    =    maybe True (< s) (rootkey z_kl)
      && maybe True (> s) (rootkey z_gr)
      && is_suchbaum z_kl
      && is_suchbaum z_gr

-- Funktion rootkey: gibt den Wurzelschlüssel eines Suchbaums zurück
rootkey :: Suchbaum a b -> Maybe a
rootkey Leer = Nothing
rootkey (Knoten s _ _ _) = Just s

s1 = einf 10 "xy" Leer
s2 = einf 4 "uv" s1
s3 = einf 3 "c" s2
s4 = einf 6 "f" s3
s5 = einf 5 "e" s4
s6 = einf 18 "bla" s5
s7 = einf 20 "blu" s6
