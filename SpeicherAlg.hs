module SpeicherAlg(WBUCH,
  finde, -- Eq a => WBUCH a b -> a -> Maybe b
  einf,
  entf,
  leer)
    where

data WBUCH a b =
   Einf (WBUCH a b) a b |
   Leer
   deriving Show
-- Woerterbuch wird als algebraischer Datentyp
-- direkt nach der Spezifikation
-- dargestellt.

finde::  Eq a => WBUCH a b -> a -> Maybe b
finde (Einf w s t) x
  | s==x   = Just t
  | otherwise = finde w x

finde Leer x = Nothing

einf:: Eq a => WBUCH a b -> a -> b -> WBUCH a b
einf = Einf

entf:: Eq a => WBUCH a b -> a -> WBUCH a b
entf Leer _ = Leer
entf (Einf wbuch s w) x
    | x == s    = entf wbuch x
    | otherwise = Einf (entf wbuch x) s w

leer:: WBUCH a b
leer = Leer

-- Folgende Bedingungen sollen gelten:
{-
finde (einf w s t) x
  s==x   = Just t
  otherwise = finde w x

finde (entf w s) x
  s==x   = Nothing
  otherwise = finde w x

finde leer x = Nothing
-}
