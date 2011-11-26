-- Typ Ausdruck: arithmetischer Ausdruck mit Multiplikation und Addition
data Ausdruck =
      Kons Int
    | Var  String
    | Plus Ausdruck Ausdruck
    | Mal  Ausdruck Ausdruck
    deriving Show

-- Funktion evaluate: wertet einen arithmetischen Ausdruck vom Typ Ausdruck
-- aus und gibt das Ergebnis zurück.
evaluate :: Ausdruck -> Int
evaluate (Kons i) = i
evaluate (Plus a1 a2) = evaluate a1 + evaluate a2
evaluate (Mal  a1 a2) = evaluate a1 * evaluate a2
