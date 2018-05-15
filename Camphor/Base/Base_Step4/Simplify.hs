{-# LANGUAGE FlexibleContexts ,NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{- Desugaring -}
module Camphor.Base.Base_Step4.Simplify
(simple4
,Set4,Expr(..),Tree(..)
) where

import Camphor.Base.Base_Step4.IdentFactory
import Camphor.SafePrelude 

data Expr c = Null 
  | DEF Ident2 | DEL Ident2 | REA Ident2 | WRI Ident2 
  | COM String | NUL String | AS0 Ident2 | ADD Ident2 Integer 
  | WHI Ident2 c | BLO c deriving (Show)

newtype Tree = Ns [Expr Tree] deriving(Show)
type Set4 =     Expr Tree

simple4 :: [Set4] -> [Set4]
simple4 [] = []
simple4 (x@(ADD _ _):y@Null   :xs) = y:simple4(x:xs)
simple4 (x@(ADD _ _):y@(COM _):xs) = y:simple4(x:xs)
simple4 (x@(ADD _ _):y@(NUL _):xs) = y:simple4(x:xs)
simple4 (ADD ide num:y@(ADD _ _):ADD ide3 num3:xs) 
 | ide == ide3 = simple4 $ ADD ide (num+num3) : y : xs
simple4 (ADD ide num:ADD ide2 num2:xs) 
 | ide == ide2 = simple4 $ ADD ide (num+num2) : xs
simple4 (x:xs) = x:simple4 xs