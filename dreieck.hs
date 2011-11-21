ist_dreieck :: Float -> Float -> Float -> Bool
ist_dreieck x y z = ((x + y) > z) && ((y + z) > x) && ((x + z) > y)
