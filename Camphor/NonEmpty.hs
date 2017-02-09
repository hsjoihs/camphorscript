{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.NonEmpty
(NonEmpty(..)

,cons
-- ,snoc
,snoc2
-- ,append
,last'
,shiftPair
,searchBy
,nE
,init'
)where
import Camphor.SafePrelude 
import qualified Data.Traversable as T
import qualified Data.Foldable as T


data NonEmpty a = (:|)a [a] deriving(Show,Eq,Ord)


 
instance T.Traversable NonEmpty where
 traverse up (x :| []    ) = (:|[]) <$> up x
 traverse up (x :| (y:ys)) = cons <$> up x <*> T.traverse up (y :| ys)

instance T.Foldable NonEmpty where
 foldMap = T.foldMapDefault 


infixr 5 :|
infixr 5 `cons`
infixr 5 `append`
-- infixl 5 `snoc`
infixl 5 `snoc2`



searchBy :: (a -> Maybe b) -> NonEmpty a -> Maybe b
searchBy f (x :| []     ) = f x
searchBy f (x :| (x2:xs)) = case f x of
 Nothing -> searchBy f (x2:|xs)
 Just b  -> Just b

instance Functor NonEmpty where
 fmap f (y :| ys) = f y :| map f ys

instance Applicative NonEmpty where
 pure = nE
 (<*>) = ap
 
instance Monad NonEmpty where
 return = nE
 m >>= f = concat'(fmap f m)
 
concat' :: NonEmpty(NonEmpty a) -> NonEmpty a
concat' (xs:|[]) = xs 
concat' (xs:|(xs2:xss)) = xs `append` concat' (xs2:|xss)

cons :: a -> NonEmpty a -> NonEmpty a
{-# INLINE cons #-}
x `cons` (y :| ys) = x :| (y:ys)

-- snoc :: NonEmpty a -> a -> NonEmpty a
-- (y :| ys) `snoc` x = y :| (ys++[x])

snoc2 :: [a] -> a -> NonEmpty a
[] `snoc2` a = a :| []
(x:xs) `snoc2` a = x :| (xs++[a])

append :: NonEmpty a -> NonEmpty a -> NonEmpty a
(x:|[]     ) `append` ys = x `cons` ys
(x:|(x2:xs)) `append` ys = x `cons` ((x2:|xs) `append` ys)


last' :: NonEmpty a -> a
last' (x :| [])     = x
last' (_ :| (y:ys)) = last' (y :| ys)

init' :: NonEmpty a -> [a]
init' (_ :| [])     = []
init' (x :| (y:ys)) = x : init'(y:|ys)

shiftPair :: NonEmpty (a,b) -> (a, [(b,a)]      , b  )
shiftPair ( (a,b) :| []    ) = (a, []           , b  )
shiftPair ( (a,b) :| (x:xs)) = (a, (b,top) : mid, bot) where (top,mid,bot) = shiftPair(x :| xs) 



nE :: a -> NonEmpty a
{-# INLINE nE #-}
nE = (:|[])