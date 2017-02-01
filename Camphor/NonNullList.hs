module Camphor.NonNullList
(NNList(..)
,toList
)where

data NNList a = (:|)a [a] deriving(Show)

toList :: NNList a -> [a]
toList (x :| xs) = x:xs