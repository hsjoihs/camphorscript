{-# LANGUAGE FlexibleContexts ,NoImplicitPrelude, GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.InfList
(InfList()
,update
,upd
,at
)where
import Camphor.SafePrelude
import Data.List(genericIndex) -- scary
newtype InfList a = I (ZipList a) deriving(Functor,Applicative)

upd :: (a -> a) -> (Integer,InfList a) -> (Integer,InfList a)
upd f (i,xs) = (i,update f i xs)

update :: (a -> a) -> Integer -> InfList a -> InfList a
update f i (I(ZipList xs)) = let (l,c:r) = genericSplitAt i xs in I(ZipList(l ++ f c:r)) 

at :: InfList a -> Integer -> a
I(ZipList xs) `at` i =  xs `genericIndex` i