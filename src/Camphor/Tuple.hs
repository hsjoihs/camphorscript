{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module Camphor.Tuple
(fst,snd,trd
,pt1,pt2,pt3
,md1,md2,md3
,Fst,Snd,Trd
)where
import Prelude hiding(fst,snd)

-- Inspired by http://hackage.haskell.org/package/tuple-0.2.0.1/docs/

class Fst a b | a -> b where 
 fst :: a -> b
 pt1 :: a -> b -> a
 md1 :: (b -> b) -> a -> a
instance Fst (a,b) a where 
 fst (a,_)   = a
 pt1 (_,b) a = (a,b)
 md1 f (a,b) = (f a,b)
instance Fst (a,b,c) a where 
 fst (a,_,_)   = a
 pt1 (_,b,c) a = (a,b,c)
 md1 f (a,b,c) = (f a,b,c)

class Snd a b | a -> b where
 snd :: a -> b
 pt2 :: a -> b -> a
 md2 :: (b -> b) -> a -> a
instance Snd (a,b) b where
 snd (_,b)   = b
 pt2 (a,_) b = (a,b)
 md2 f (a,b) = (a,f b)
instance Snd (a,b,c) b where
 snd (_,b,_)   = b
 pt2 (a,_,c) b = (a,b,c)
 md2 f (a,b,c) = (a,f b,c)
 
class Trd a b | a -> b where
 trd :: a -> b
 pt3 :: a -> b -> a
 md3 :: (b -> b) -> a -> a
instance Trd (a,b,c) c where
 trd (_,_,c)   = c
 pt3 (a,b,_) c = (a,b,c)
 md3 f (a,b,c) = (a,b,f c)