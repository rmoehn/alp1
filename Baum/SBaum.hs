module Baum.SBaum
where

data SBaum a b
    = Leer
      | Knoten a b (SBaum a b) (SBaum a b)
    deriving Show

-- Funktion finde: Sucht einen Schlüssel in einem SBaum und gibt dessen
-- Wert zurück
finde :: Ord a => a -> SBaum a b -> Maybe b
finde _ Leer = Nothing
finde key (Knoten s w z_kl z_gr)
    | key < s   = finde key z_kl
    | key > s   = finde key z_gr
    | otherwise = Just w

-- Funktion einf: fügt ein Schlüssel-Wert-Paar in einen SBaum ein
einf :: Ord a => a -> b -> SBaum a b -> SBaum a b
einf s w Leer = Knoten s w Leer Leer
einf s w (Knoten k_s k_w z_kl z_gr)
    | s < k_s  = Knoten k_s k_w (einf s w z_kl) z_gr
    | s > k_s  = Knoten k_s k_w z_kl (einf s w z_gr)
    | s == k_s = Knoten k_s k_w z_kl z_gr

-- Funktion einf_list: fügt eine Liste von Schlüssel-Wert-Paaren in einen
-- SBaum ein
einf_list :: Ord a => [(a, b)] -> SBaum a b -> SBaum a b
einf_list []         baum   = baum
einf_list ((s, w):sws) baum = einf_list sws (einf s w baum)

-- Funktion is_suchbaum: überprüft, ob ein SBaum richtig sortiert ist
is_suchbaum :: Ord a => SBaum a b -> Bool
is_suchbaum Leer = True
is_suchbaum (Knoten s _ z_kl z_gr)
    =    maybe True (< s) (rootkey z_kl)
      && maybe True (> s) (rootkey z_gr)
      && is_suchbaum z_kl
      && is_suchbaum z_gr

-- Funktion rootkey: gibt den Wurzelschlüssel eines SBaums zurück
rootkey :: SBaum a b -> Maybe a
rootkey Leer = Nothing
rootkey (Knoten s _ _ _) = Just s

-- Funktion hoehe: gibt die Höhe eines SBaums zurück
hoehe :: SBaum a b -> Int
hoehe (Knoten _ _ zl zr) = 1 + max (hoehe zl) (hoehe zr)
hoehe Leer               = -1

-- Funktion tiefe: gibt die Tiefe eines Knotens zurück, sofern der Knoten Teil
-- des Baumes im zweiten Argument ist
tiefe :: SBaum a b -> SBaum a b -> Int
tiefe knoten baum = hoehe baum - hoehe knoten


-- SBaum in Baum mit Tiefenspeicherknoten umwandeln
data Baum a = TLeer | TKnoten a (Baum a) (Baum a)
    deriving Show

depths :: SBaum a b -> Baum Int
depths baum = depths' baum baum

depths' :: SBaum a b -> SBaum a b -> Baum Int
depths' Leer _ = TLeer
depths' (Knoten s w zl zr) ganzbaum
    = TKnoten (tiefe (Knoten s w zl zr) ganzbaum)
          (depths' zl ganzbaum)
          (depths' zr ganzbaum)

s1 = einf 10 "xy" Leer
s2 = einf 4 "uv" s1
s3 = einf 3 "c" s2
s4 = einf 6 "f" s3
s5 = einf 5 "e" s4
s6 = einf 18 "bla" s5
s7 = einf 20 "blu" s6
