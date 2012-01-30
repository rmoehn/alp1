import Hash.TupListHash
import Baum.BBaum
import Control.Monad.State (State, runState, state)

-- Funktion nummeriere: Ersetzt Daten in einem Baum durch Zahlen. Gleiche
-- Daten bekommen die gleiche Zahl
nummeriere :: Eq a => BBaum a -> BBaum Int
nummeriere (Knoten v zl zr) =

-- Funktion sym_trav: traversiert einen BBaum symmetrisch und wendet die
-- übergebene Funktion auf die Knoten an
sym_trav :: BBaum a -> gt
sym_trav Leer _ = return ()
sym_trav (Knoten v zl zr) func = do
    sym_trav zl
    func v
    sym_trav zr
