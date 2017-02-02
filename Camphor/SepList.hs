{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.SepList
(SepList(..)
,reverse
-- ,toList'
,toSeparatorList
-- ,isSingle
) where
import Camphor.SafePrelude hiding(reverse)
import qualified Camphor.SafePrelude as P
import Camphor.Listlike

data SepList s a = SepList a [(s,a)] deriving(Show,Eq,Ord)

reverse :: SepList b a -> SepList b a
reverse (SepList t ts) = uncurry SepList(reverse' (t,ts)) 
 where
  reverse' :: (a,[(b,a)]) -> (a,[(b,a)])
  reverse' k = (e,P.reverse rev)
   where{
 (e,rev) = tmp k;
 tmp (a,[])= (a,[]);
 tmp (a,(b,a2):xs) = (q,(b,a):w) where (q,w) = reverse' (a2,xs)
   }
   
instance Listlike(SepList b) where
 toList' (SepList v xs) = v:map snd xs 

-- isSingle :: SepList a b -> Bool
-- isSingle (SepList _ []) = True
-- isSingle _ = False 
 
toSeparatorList :: SepList b a -> [b]
toSeparatorList (SepList _ xs) = map fst xs

nazo :: (a -> SepList s b) -> (s,a) -> [(s,b)]
nazo f (s,a1) = (s,a2):zs where SepList a2 zs = f a1 

append :: SepList s a -> [(s,a)] -> SepList s a
SepList t ts `append` us = SepList t (ts++us)
 
instance Monad (SepList c) where
 return a = SepList a []
 SepList a xs >>= f = f a `append` (xs >>= nazo f)

instance Functor (SepList c) where
 fmap = liftM 
 
instance Applicative (SepList c)where
 pure = return
 (<*>) = ap
 
{-

proofs that SepList c is a monad

return a >>= f
SepList a [] >>= f
f a `append` ([] >>= nazo f)
f a `append` []
f a

nazo return (s,a1)  
(s,a2):zs where SepList a2 zs = return a1 
(s,a2):zs where SepList a2 zs = SepList a1 []
(s,a1):[]
return (s,a1)

SepList a xs >>= return
return a `append` (xs >>= nazo return)
SepList a [] `append` (xs >>= return)
SepList a [] `append` xs 
SepList a xs 

(SepList a xs `append` ys) >>= g
(SepList a (xs ++ ys)) >>= g
g a `append` ((xs ++ ys) >>= nazo g)
g a `append` ((xs >>= nazo g) ++ (ys >>= nazo g))
(g a `append` (xs >>= nazo g)) `append` (ys >>= nazo g)
(SepList a xs >>= g) `append` (ys >>= nazo g)

let SepList w ws = f a1
let SepList a2 vs = g w
nazo (\x -> f x >>= g) (s,a1)
(s,a2):zs where SepList a2 zs = f a1 >>= g
(s,a2):zs where SepList a2 zs = SepList w ws >>= g
(s,a2):zs where SepList a2 zs = g w `append` (ws >>= nazo g)
(s,a2):zs where SepList a2 zs = SepList a2 vs `append` (ws >>= nazo g)
(s,a2):zs where zs = vs ++ (ws >>= nazo g)
(s,a2):(vs ++ (ws >>= nazo g))
((s,a2):vs) ++ (ws >>= nazo g)
nazo g (s,w) ++ (ws >>= nazo g)
([(s,w)] >>= nazo g) ++ (ws >>= nazo g)
((s,w):ws)  >>= nazo g
nazo f (s,a1) >>= nazo g
(\x -> nazo f x >>= nazo g) (s,a1)

(SepList a xs >>= f) >>= g
(f a `append` (xs >>= nazo f)) >>= g
(f a >>= g) `append` ((xs >>= nazo f) >>= nazo g)
(f a >>= g) `append` (xs >>= (\x -> nazo f x >>= nazo g))
(f a >>= g) `append` (xs >>= nazo (\x -> f x >>= g))
(\x -> f x >>= g) a `append` (xs >>= nazo (\x -> f x >>= g))
SepList a xs >>= (\x -> f x >>= g)


-} 