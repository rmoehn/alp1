import Num_Utils

main = print (perf_zahlen 1 1000000000)

-- Funktion num_larger_teilersum: gibt die Liste der Zahlen zwischen m und n
-- zurück, bei denen die Summer der echten Teiler größer ist als die Zahl
num_larger_teilersum :: Integer -> Integer -> [Integer]
num_larger_teilersum m n = [x | x <- [m..n], x < sum (teiler x)]

-- Funktion perf_zahlen: gibt die Liste der perfekten Zahlen zwischen m und n
-- zurück
perf_zahlen :: Integer -> Integer -> [Integer]
perf_zahlen m n = [x | x <- [m..n], x == sum (teiler x)]
