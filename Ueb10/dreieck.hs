-- Funktion orient: Gibt die Orientierung eines durch seine Punkte
-- beschriebenen Dreiecks zurück. -- -1 bei negativ, 1 bei positiv, 0 bei
-- Dreieck mit Fläche 0.
orient :: Fractional a => [(a, a)] -> a
orient points = signum ((x3*y2 - x2*y3) / 2)
    where
    (x1, y1)             = head points
    transpoints          = map (\(x, y) -> (x-x1, y-y1)) points
    [(x2, y2), (x3, y3)] = tail transpoints
