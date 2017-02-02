{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Oper
(Oper(),wrap,unOp
)where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Data.Char(isSpace)
newtype Oper = Op String deriving(Ord)

delspace :: String -> String
delspace = filter(not . isSpace)

instance Show Oper where
 show (Op str) = delspace str
 
instance Eq Oper where
 (Op str)==(Op str2) = delspace str == delspace str2
 
wrap :: String -> Oper
wrap str = Op (delspace str)

unOp :: Oper -> String
unOp (Op str) = str
 