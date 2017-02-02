{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.Listlike
(Listlike,toList'
)
where
import Camphor.SafePrelude 

class Listlike f where
 toList' :: f a -> [a]
 
instance Listlike [] where toList' = id