{-# OPTIONS -Wall #-}
module Camphor.NonEmpty
(NonEmpty(..)
,toList
,cons
,last'
,shiftPair
,minimumBy'
,minimumsBy
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

minimumBy' :: (a -> a -> Ordering) -> NonEmpty a -> a
minimumBy' cmp (k :| ks) = foldl minBy k ks
 where minBy x y = case cmp x y of GT -> y; _ -> x

minimumsBy :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
minimumsBy cmp_ (x_ :| xs_) = go2 cmp_ xs_ (x_:|[])
 where 
  go2 _ [] acc = acc
  go2 cmp (x:xs) acc = go cmp (x:|xs) acc
  go cmp (x :| xs) acc@(y :| _) = case cmp x y of
   GT -> go2 cmp xs acc --- x > y
   EQ -> go2 cmp xs (x `cons` acc) -- x == y
   LT -> go2 cmp xs (x :| []) -- x < y