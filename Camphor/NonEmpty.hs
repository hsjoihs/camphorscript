{-# OPTIONS -Wall #-}
module Camphor.NonEmpty
(NonEmpty(..)
,toList
,cons
,last'
,shiftPair
)where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
data NonEmpty a = (:|)a [a] deriving(Show)

infixr 5 :|
infixr 5 `cons`

toList :: NonEmpty a -> [a]
toList (x :| xs) = x:xs

cons :: a -> NonEmpty a -> NonEmpty a
x `cons` (y :| ys) = x :| (y:ys)

last' :: NonEmpty a -> a
last' (x :| [])     = x
last' (_ :| (y:ys)) = last' (y :| ys)

shiftPair :: NonEmpty (a,b) -> (a, [(b,a)]      , b  )
shiftPair ( (a,b) :| []    ) = (a, []           , b  )
shiftPair ( (a,b) :| (x:xs)) = (a, (b,top) : mid, bot) where (top,mid,bot) = shiftPair(x :| xs) 