{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.TailSepList
(TailSepList(..)
)where
import Camphor.SafePrelude
import Camphor.Listlike


newtype TailSepList o v = TSL {unTSL :: [(o,v)]} deriving(Show,Eq,Ord)

instance Functor (TailSepList o) where
 fmap f (TSL xs) = TSL $ map (second f) xs
 
instance Listlike (TailSepList o) where
 toList' (TSL xs) = map snd xs