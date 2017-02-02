{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.SepList
(SepList(..),reverse,toList,toSeparatorList
) where
import Camphor.SafePrelude hiding(reverse)
import qualified Camphor.SafePrelude as P

newtype SepList s a = SepList{ unSep :: (a,[(s,a)]) } deriving(Show,Eq)

instance Functor (SepList c) where
 fmap f (SepList(a,xs)) = SepList(f a,[ (c1,f a1) | (c1,a1) <- xs ]) 

reverse :: SepList b a -> SepList b a
reverse (SepList l) = SepList(reverse' l) 
 where
  reverse' :: (a,[(b,a)]) -> (a,[(b,a)])
  reverse' k = (e,P.reverse rev)
   where{
 (e,rev) = tmp k;
 tmp (a,[])= (a,[]);
 tmp (a,(b,a2):xs) = (q,(b,a):w) where (q,w) = reverse' (a2,xs)
   }
   
toList :: SepList b a -> [a]
toList (SepList(v,xs)) = v:map snd xs 

toSeparatorList :: SepList b a -> [b]
toSeparatorList (SepList(_,xs)) = map fst xs