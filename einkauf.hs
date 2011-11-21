import Hash
import Finanz

-- Typsynonyme für Einkaufs- und Preislisten
type Einkaufsliste = [(String, Float)]
type Einkaufsitem  = (String, Float)
type Preisliste    = [(String, Float)]

-- Beispiellisten
einkauf1 :: Einkaufsliste
einkauf1  = [("Brot", 1), ("Milch", 3), ("Äpfel", 7), ("Froschschenkel", 1)]

preise :: Preisliste
preise  = [("Milch", 0.89), ("Äpfel", 0.3), ("Brot", 2.75)]

-- Funktion preis: gibt den Gesamtpreis einer Einkaufliste und eine Liste der
-- Artikel aus, deren Grundpreis nicht gefunden wurde (Paar)
preis :: Preisliste -> Einkaufsliste -> (Float, [String])
preis preise items = (
    -- Gesamtpreis als Summe der Einzelpreise berechnen.
    priceround (sum (map (itempreis preise) items)),
    [art | (art, quant) <- items, itempreis preise (art, quant) == 0]
    )

-- Funktion itempreis: gibt den Preis für einen Punkt auf einer Einkaufsliste
-- zurück
itempreis :: Preisliste -> Einkaufsitem -> Float
itempreis preise (art, quant)
    -- Artikel, die nicht in der Preisliste sind, bekommen den Preis 0.
    | exists preise art == False = 0
    | otherwise                  = quant * (value preise art)
