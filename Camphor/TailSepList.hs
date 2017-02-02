{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.TailSepList
(TailSepList(..)
)where
import Camphor.SafePrelude
import Camphor.Listlike


newtype TailSepList o v = TSL {unTSL :: [(o,v)]} deriving(Show,Eq)

instance Functor (TailSepList o) where
 fmap f (TSL xs) = TSL $ map (\(o,v) -> (o,f v)) xs
 
instance Listlike (TailSepList o) where
 toList' (TSL xs) = map snd xs