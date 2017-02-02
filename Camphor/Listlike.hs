{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.Listlike
(Listlike,toList',length'
)
where
import Camphor.SafePrelude 

class Listlike f where
 toList' :: f a -> [a]

 
instance Listlike [] where 
 toList' = id

length' :: Listlike f => f a -> Int
length' = length . toList' 