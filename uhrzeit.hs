import Num_Utils

data Uhrzeit = Zeit {
    h :: Int,
    m :: Int
}

-- wie man Zeiten richtig ausgeben sollte
instance Show Uhrzeit where
    show (Zeit h m) = show h ++ ":" ++ pad_int 2 '0' m

instance Enum Uhrzeit where
    toEnum x = Zeit (mod (h + div m 60) 24) (mod m 60)
        where
        h = div x 100
        m = mod x 100
    fromEnum (Zeit h m) = 100 * (mod (h + div m 60) 24) + (mod m 60)
--    succ (Zeit h m)
--        | m == 59 = (Zeit (mod (h+1) 24) 0)
--        | otherwise = (Zeit h (m+1))
--    pred (Zeit h m)
--        | m == 00 = (Zeit (mod (h-1) 24) 59)
--        | otherwise = (Zeit h (m-1))
