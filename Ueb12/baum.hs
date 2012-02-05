import Hash.TupListHash
import Baum.BBaum
import Control.Monad.State (State, runState, state)

-- Funktion numeriere: Ersetzt Daten in einem Baum durch Zahlen. Gleiche
-- Daten bekommen die gleiche Zahl
numeriere :: Eq a => BBaum a -> BBaum Int
numeriere (Knoten v zl zr) = do
    num_v  <- numeriere_knoten v
    num_zl <- numeriere_zweig  zl
    num_zr <- numeriere_zweig  zr
    return (Node num_v num_zl num_zr)
