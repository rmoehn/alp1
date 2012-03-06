import List.Utils
import Baum.SBaum
import Num.Utils

-- Funktion alle_suchb: generiert alle Suchbäume mit den Schlüsseln 1 .. n
alle_suchb :: Int -> [SBaum Int Int]
alle_suchb n = map (\perm -> einf_list perm Leer) perms_w_keys
    where
    perms_w_keys = map (\perm -> zip perm $ repeat 0) $ permutationen n

suchb_10       = alle_suchb 10 :: [SBaum Int Int]
baumhoehen     = (map $ fromIntegral.hoehe) suchb_10
hoehenmittelw  = avg baumhoehen
hoehenstat     = count_occ baumhoehen 10
