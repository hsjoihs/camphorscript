{-# LANGUAGE FlexibleContexts ,NoImplicitPrelude, GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.InfList
(InfList()
,update
,upd
,at
,initial
)where
import Camphor.SafePrelude
import qualified Data.Map as M

newtype InfList a = I (M.Map Integer a)

upd :: (Num a) => (a -> a) -> (Integer,InfList a) -> (Integer,InfList a)
upd f (i,xs) = (i,update f i xs)

initial :: (Num a) => InfList a
initial = I M.empty

update :: (Num a) => (a -> a) -> Integer -> InfList a -> InfList a
update f i m@(I xs) = I $ M.insert i (f(m `at` i)) xs

at :: (Num a) => InfList a -> Integer -> a
I xs `at` i = fromMaybe 0 $ M.lookup i xs 