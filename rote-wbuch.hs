--import SpeicherFunk
--import SpeicherList
--import SpeicherSuchbaum ...
import SpeicherList

w:: WBUCH Int String
w = leer

w1 = einf w 3 "xx"
w2 = einf w1 5 "gg"
w3 = entf w2 3
w4 = einf w3 5 "neu"
