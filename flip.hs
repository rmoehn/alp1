import Lists
import String_Utils
import Num_Utils

teststring :: String
teststring = "\\     +-->\n \\    |\n  +---+\n"

-- Funktion flipH: spiegelt ein durch einen String gegebenes Bild horizontal
flipH :: String -> IO ()
flipH image = (
        putStr          -- ausgeben
      . join "\n"       -- wieder verbinden
      . map (
            rem_trail_ws-- Whitespaces entfernen, die dann am Ende der Zeile
                        -- stünden
          . pad_string max_linelength ' '
                        -- Was hinten übersteht, muss bei gespiegelten auch
                        -- vorn überstehen.
          . reverse     -- spiegeln
        )
    ) lines
    where lines          = trennen "\n" image  -- Zeilen auftrennen
          max_linelength = maximum (map length lines)
              -- längste Zeile bestimmt die Einrückung der anderen

-- Funktion flipV: spiegelt ein durch einen String gegebenes Bild vertikal
flipV :: String -> IO ()
flipV image = putStr ((join "\n" . reverse . split "\n") image ++ "\n")
