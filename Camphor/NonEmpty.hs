{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.NonEmpty
(NonEmpty(..)
,toList'
,cons,snoc,snoc2,append
,last'
,shiftPair
,minimumBy'
,minimumsBy
,concat'
,searchBy
,nE
)where
import Camphor.SafePrelude 
import Camphor.Listlike
data NonEmpty a = (:|)a [a] deriving(Show)

infixr 5 :|
infixr 5 `cons`
infixr 5 `append`
infixl 5 `snoc`
infixl 5 `snoc2`

searchBy :: (a -> Maybe b) -> NonEmpty a -> Maybe b
searchBy f (x :| []     ) = f x
searchBy f (x :| (x2:xs)) = case f x of
 Nothing -> searchBy f (x2:|xs)
 Just b  -> Just b


instance Listlike NonEmpty where
 toList' (x :| xs) = x:xs

cons :: a -> NonEmpty a -> NonEmpty a
{-# INLINE cons #-}
x `cons` (y :| ys) = x :| (y:ys)

snoc :: NonEmpty a -> a -> NonEmpty a
(y :| ys) `snoc` x = y :| (ys++[x])

snoc2 :: [a] -> a -> NonEmpty a
[] `snoc2` a = a :| []
(x:xs) `snoc2` a = x :| (xs++[a])

append :: NonEmpty a -> NonEmpty a -> NonEmpty a
(x:|[]     ) `append` ys = x `cons` ys
(x:|(x2:xs)) `append` ys = x `cons` ((x2:|xs) `append` ys)


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
   
concat' :: NonEmpty(NonEmpty a) -> NonEmpty a
concat' (xs:|[]) = xs 
concat' (xs:|(xs2:xss)) = xs `append` (concat' (xs2:|xss))

nE :: a -> NonEmpty a
{-# INLINE nE #-}
nE = (:|[])