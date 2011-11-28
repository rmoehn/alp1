-- Funktion fibo: gibt die n-te Fibonacci-Zahl zurück
fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n
    | n >  0 = fibo (n-1) + fibo (n-2)
    | n <= 0 = fibo (n+2) - fibo (n+1)
