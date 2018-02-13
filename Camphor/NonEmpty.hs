{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.NonEmpty
(NonEmpty(..)

,cons
,snoc2
,last'
,shiftPair
,searchBy
,nE
,init'
)where
import Camphor.SafePrelude 
import Data.List.NonEmpty

infixl 5 `snoc2`

searchBy :: (a -> Maybe b) -> NonEmpty a -> Maybe b
searchBy f (x :| []     ) = f x
searchBy f (x :| (x2:xs)) = case f x of
 Nothing -> searchBy f (x2:|xs)
 Just b  -> Just b

snoc2 :: [a] -> a -> NonEmpty a
[] `snoc2` a = a :| []
(x:xs) `snoc2` a = x :| (xs++[a])

last' :: NonEmpty a -> a
last' = Data.List.NonEmpty.last

init' :: NonEmpty a -> [a]
init' = Data.List.NonEmpty.init

shiftPair :: NonEmpty (a,b) -> (a, [(b,a)]      , b  )
shiftPair ( (a,b) :| []    ) = (a, []           , b  )
shiftPair ( (a,b) :| (x:xs)) = (a, (b,top) : mid, bot) where (top,mid,bot) = shiftPair(x :| xs) 

nE :: a -> NonEmpty a
nE = return
