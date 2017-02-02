{-# OPTIONS -Wall #-}
module Camphor.NonEmpty
(NonEmpty(..)
,toList
,cons
)where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
data NonEmpty a = (:|)a [a] deriving(Show)

infixr 5 :|
infixr 5 `cons`

toList :: NonEmpty a -> [a]
toList (x :| xs) = x:xs

cons :: a -> NonEmpty a -> NonEmpty a
x `cons` (y :| ys) = x :| (y:ys)