module String_Utils (
    rem_trail_ws,
)
where

-- Funktion rem_trail_ws: entfernt Whitespace am Ende eines Strings
rem_trail_ws :: String -> String
rem_trail_ws ""  = ""
rem_trail_ws string
    | last string == ' ' = rem_trail_ws (init string)
    | otherwise          = string
