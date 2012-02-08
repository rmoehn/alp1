import System.Random
import Baum.SBaum
import Num.Utils

-- Funktion gen_rand_nrs: generiert eine Liste von Float-Zufallszahlen
gen_rand_nrs :: Int -> Int -> [Float]
gen_rand_nrs nr seed = take nr $ randoms (mkStdGen seed)

-- Funktion : erzeugt einen zufälligen Suchbaum mit n Knoten
gen_rand_sbaum :: Num b => Int -> Int -> SBaum Float b
gen_rand_sbaum nr seed
    = einf_list (map (\x -> (x, 0)) $ gen_rand_nrs nr seed) Leer

-- Funktion gen_rand_sbaeume: generiert eine Liste von b_nr Suchbäumen mit
-- jeweils n_nr Knoten
gen_rand_sbaeume :: Num b => Int -> Int -> Int -> [SBaum Float b]
gen_rand_sbaeume 0 _ _ = []
gen_rand_sbaeume b_nr n_nr seed
    = gen_rand_sbaum n_nr seed : gen_rand_sbaeume (b_nr - 1) n_nr (seed + 1)

-- Funktion inc_array_ind: inkrementiert den Wert bei index in einem Array um
-- 1
inc_array_ind :: Num a => Int -> [a] -> [a]
inc_array_ind index ary = firstpart ++ [val + 1] ++ secpart
    where
    val       = ary !! index
    firstpart = take index ary
    secpart   = drop (index + 1) ary

tausend_baeume = gen_rand_sbaeume 1000 10 33
baumhoehen     = (map hoehe) tausend_baeume
hoehenmittelw  = avg baumhoehen
hoehenstat     = count_occ baumhoehen 10

-- Funktion count_occ: Berechnet für eine Eingabeliste mit Zahlen 0 .. maxval,
-- wie oft die Zahl darin vorkommt. Ausgabe ist eine Liste, bei deren Indizes
-- 0 ..  n die Anzahlen stehen
count_occ :: [Int] -> Int -> [Int]
count_occ inpt_list maxval = count_occ' inpt_list (replicate maxval 0)
    where
    -- man braucht eine Anfangsliste, wo alle Anzahlen auf 0 stehen
    count_occ' []     occ_rec = occ_rec
    count_occ' (x:xs) occ_rec = inc_array_ind x (count_occ' xs occ_rec)

