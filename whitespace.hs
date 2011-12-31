import IO.Prompt
import String.Utils

-- Funktion tidy_file_trail_ws: entfernt anhängende Leerzeichen in einer vom
-- Menschen vor dem Computer angegebenen Datei
tidy_file_trail_ws :: IO ()
tidy_file_trail_ws = do
    -- Eingabe- und Ausgabedateinamen abfragen
    infile_name  <- prompt "Name der zu säubernden Datei? "
    outfile_name <- prompt "Wohin soll's geschrieben werden? "
    -- Eingabedatei einlesen
    untidy_contents <- readFile infile_name
    -- anhängende Leerzeichen entfernen und in Ausgabedatei schreiben
    writeFile outfile_name ( (
         unlines
         . map rem_trail_ws
         . lines
         ) untidy_contents)
