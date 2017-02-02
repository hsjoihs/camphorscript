{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.TailSepList
(TailSepList(..)
)where
import Camphor.SafePrelude
import qualified Data.Traversable as T
import qualified Data.Foldable as T

newtype TailSepList o v = TSL {unTSL :: [(o,v)]} deriving(Show,Eq,Ord)

instance Functor (TailSepList o) where
 fmap f (TSL xs) = TSL $ map (second f) xs
 
instance T.Traversable (TailSepList s) where
 traverse _  (TSL []         ) = pure $ TSL []
 traverse up (TSL ((o,v):ovs)) = cons <$> fmap (\m ->(o,m)) (up v) <*> T.traverse up (TSL ovs)

instance T.Foldable (TailSepList s) where
 foldMap = T.foldMapDefault  

cons :: (o,v) -> TailSepList o v -> TailSepList o v
cons (o,v) (TSL ovs) = TSL $ (o,v):ovs