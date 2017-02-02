{-# OPTIONS -Wall #-}
module Camphor.NonEmpty
(NonEmpty(..)
,toList
,cons
)where

data NonEmpty a = (:|)a [a] deriving(Show)

infixr 5 :|
infixr 5 `cons`

toList :: NonEmpty a -> [a]
toList (x :| xs) = x:xs

cons :: a -> NonEmpty a -> NonEmpty a
x `cons` (y :| ys) = x :| (y:ys)