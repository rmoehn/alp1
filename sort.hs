-- Funktion selsort: sortiert eine Liste mit dem Selection-Sort-Verfahren
selsort :: Ord a => [a] -> [a]
selsort []    = []
selsort elems = minelem : selsort (filter (/=minelem) elems)
    where minelem = minimum elems
