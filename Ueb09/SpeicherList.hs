module SpeicherList where

data WBUCH a b = WBuch [(a,b)]
     deriving Show
-- Woerterbuch wird durch eine Liste von Paaren
-- dargestellt.

finde::  Eq a => WBUCH a b -> a -> Maybe b
finde (WBuch []) x = Nothing
finde (WBuch ((s,t):w)) x =
  if s==x then Just t else finde (WBuch w) x

einf:: Eq a => WBUCH a b -> a -> b -> WBUCH a b
-- einf (WBuch w) s t = WBuch ((s,t):w)
einf (WBuch w) s t = WBuch ((s,t):[(a,b) | (a,b)<-w, a/=s])

entf:: Eq a => WBUCH a b -> a -> WBUCH a b
entf (WBuch w) s = WBuch w'
   where w'= [(a,b) | (a,b)<-w, a/=s]

leer:: WBUCH a b
leer = WBuch []

-- Funktion anzahl: ermittelt die Anzahl der im Wörterbuch enthaltenen
-- Schlüssel
anzahl :: WBUCH a b -> Int
anzahl (WBuch liste) = length liste


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
