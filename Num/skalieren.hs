graphik = [
    "D    +-->",
    " D   |   ",
    "  +--+   "
    ]

-- Funktion skaliereH: skaliert eine Buchstabengraphik horizontal mit einem
-- ganzzahligen Faktor
skaliereH :: Int -> [String] -> [String]
skaliereH faktor graphik = map (concat . (map $ replicate faktor)) graphik

-- Funktion skaliereV: skaliert eine Buchstabengraphik vertikal mit einem
-- ganzzahligen Faktor
skaliereV :: Int -> [String] -> [String]
skaliereV faktor graphik = concat $ map (replicate faktor) graphik

-- Funktion skaliere: skaliert eine Buchstabengraphik mit einem ganzzahligen
-- Faktor
skaliere :: Int -> [String] -> [String]
skaliere faktor graphik = ((skaliereH faktor) . (skaliereV faktor)) graphik

