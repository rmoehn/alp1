-- Funktion runl_enc: führt ein Runlength-Encoding auf einer Liste von
-- Objekten aus. -- "aaaabbaaa" wird zu [(4, 'a'), (2, 'b'), (3, 'a')]
runl_enc :: Eq a => [a] -> [(Int, a)]
runl_enc []     = []
runl_enc (x:xs) = [(num_eq_elems + 1, x)] ++ runl_enc (drop num_eq_elems xs)
    -- ermittelt die Anzahl aufeinanderfolgender gleicher Elemente in einer
    -- Liste vermindert um 1
    where num_eq_elems = length (takeWhile (== x) xs)

-- Funktion runl_dec: führt ein Runlength-Decoding auf einer von runl_enc
-- erstellten Liste aus
runl_dec :: [(Int, a)] -> [a]
runl_dec [] = []
runl_dec ((num, obj):xs) = (replicate num obj) ++ runl_dec xs

-- Funktion runl_enc_alph: führt ein Runlength-Encoding auf einer keine
-- Ziffern enthaltenden Zeichenkette aus. -- "aaaabbaaa" wird zu "4a2b3a"
runl_enc_alph :: String -> String
runl_enc_alph string = enc_to_encalph (runl_enc string)

-- Funktion enc_to_encalph: wandelt eine von runl_enc enkodierte Liste in das
-- Format von runl_enc_alph um
enc_to_encalph :: [(Int, Char)] -> String
enc_to_encalph [] = ""
enc_to_encalph ((num, char):xs) = show num ++ [char] ++ (enc_to_encalph xs)

-- Funktion runl_dec_alph: führt ein Runlength-Decoding auf einem von
-- runl_enc_alph erstellten String aus
runl_dec_alph :: String -> String
runl_dec_alph string = runl_dec (encalph_to_enc string)

-- Funktion encalph_to_enc: wandelt eine von runl_enc_alph enkodierte Liste in
-- das Format von runl_enc um
encalph_to_enc :: String -> [(Int, Char)]
encalph_to_enc []     = []
encalph_to_enc string =
    [ (
        read (take cnt_length string), -- Anzahl für das kommende Zeichen
        string!!cnt_length             -- kommendes Zeichen
    ) ]
    ++ encalph_to_enc (drop (cnt_length + 1) string) -- Rest des Strings
    where cnt_length = length (takeWhile is_number string)

-- Funktion is_number: testet, ob ein übergebenes Zeichen eine Ziffer ist
is_number :: Char -> Bool
is_number zeichen = elem (fromEnum zeichen) [48..57]

