woerter :: [String]
woerter = ["Europa","Schulden","Schulden","Griechenland","Sonne","Italien",
           "Angela Merkel","Schulden"]

wort :: String
wort = "Schulden"

ersetzeWort :: String -> String -> String
ersetzeWort unwort w
    | unwort == w = replicate (length unwort) '*'
    | otherwise   = w

zensiere1 :: String -> [String] -> [String]
zensiere1 unwort = map (ersetzeWort unwort)

zensiere5 :: String -> [String] -> [String]
zensiere5 = map . ersetzeWort

zensiere6 :: String -> [String] -> [String]
zensiere6 unwort = (map . ersetzeWort) unwort
