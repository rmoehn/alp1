import Num_Utils

-- Liste der Faktoren des kleinen Einmaleins
faktoren :: [Int]
faktoren = [1..1]

-- Funktion print_produkte: gibt die Produkte der Elemente der Liste als
-- Tabelle aus (kleines Einmaleins für Liste [0..10]
print_produkte :: [Int] -> IO ()
print_produkte faktoren = putStr (prt_tabhead faktoren
                                  ++ prt_tabbody faktoren 1)

-- Funktion prt_tabbody: gibt die i-te Zeile der Tabelle aus; bei i =
-- -1 den Tabellenkopf, bei i = 0 die Trennung zwischen Kopf und
-- Körper
prt_tabbody :: [Int] -> Int -> String
prt_tabbody fakt i
    | i > length fakt  = ""
    | otherwise        = prt_tabzeile [i * x | x <- fakt]
                                      (max_stellen fakt)
                         ++ prt_tabbody fakt (i+1)

-- Funktion prt_tabhead: gibt den Tabellenkopf aus
prt_tabhead :: [Int] -> String
prt_tabhead fakt = prt_tabzeile fakt (max_stellen fakt)
                   ++ prt_trenner (length fakt)
                                  (max_stellen fakt)

-- Funktion prt_trenner: gibt den Trenner zwischen Kopf und Körper der
-- Tabelle aus
prt_trenner :: Int -> Int -> String
prt_trenner spalten breite = replicate (breite + 1) '-'
                             ++ "+"
                             ++ concat (replicate spalten
                                            (replicate (breite + 1)
                                                       '-'
                                            )
                                       )
                             ++ "\n"

-- Funktion max_stellen: gibt die Anzahl der Stellen des größten
-- Produkts zurück
max_stellen :: [Int] -> Int
max_stellen fakt = length (show ((maximum fakt)^2))

-- Funktion prt_tabzeile: gibt eine Zeile der Tabelle der Produkte aus
-- (bzw. Faktoren in der ersten Zeile)
prt_tabzeile :: [Int] -> Int -> String
prt_tabzeile prod stellen = pad_int (head prod) stellen ' '
                            ++ " |"
                            ++ prt_list prod (stellen + 1)
                            ++ "\n"

-- Funktion prt_list: gibt eine Liste von Zahlen in Form von
-- Tabellenspalten aus
prt_list :: [Int] -> Int -> String
prt_list [] breite     = ""
prt_list zahlen breite = pad_int (head zahlen) breite ' '
                         ++ prt_list (tail zahlen) breite
