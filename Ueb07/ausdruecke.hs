import Hash

-- Typ Ausdruck: arithmetischer Ausdruck mit Multiplikation und Addition
data Ausdruck =
      Kons Int
    | Var  String
    | Plus Ausdruck Ausdruck
    | Mal  Ausdruck Ausdruck
    deriving Show

-- Funktion evaluate: wertet einen arithmetischen Ausdruck vom Typ Ausdruck
-- aus und gibt das Ergebnis zurück.
evaluate :: Ausdruck -> [(String, Int)] -> Int
evaluate (Kons i) _ = i
evaluate (Var  v) varhash     = value varhash v
    -- schlägt den Wert der Variablen v im Pseudo-Hash varhash nach
evaluate (Plus a1 a2) varhash = evaluate a1 varhash + evaluate a2 varhash
evaluate (Mal  a1 a2) varhash = evaluate a1 varhash * evaluate a2 varhash

-- Funktion evaluatek: das gleiche wie evaluate, lässt aber nur konstante
-- Ausdrücke zu
evaluatek :: Ausdruck -> Int
evaluatek ausdruck = evaluate ausdruck []
