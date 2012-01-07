--import SpeicherFunk
--import SpeicherList
--import SpeicherSuchbaum ...
import SpeicherAlg

w:: WBUCH Int String
w = leer

w1 = einf w 3 "xx"
w2 = einf w1 5 "gg"
w3 = entf w2 3
w4 = einf w3 5 "neu"

qsort:: Ord a=>[a]->[a]

qsort [] = []
qsort [a] = [a]
qsort (a:rest) =
  qsort [x | x<-rest, x<a] ++ [a] ++
  qsort [x | x<-rest, x>=a]

l1 = [6,43,2,4,2,6,7,56,9,3,2,9]

msort:: Ord a=>[a]->[a]

msort [] = []
msort [a] = [a]
msort l = verschmelze (msort m1) (msortm m2)
  where m1 = take n' l
        m2 = drop n' l
        n' = div n 2


