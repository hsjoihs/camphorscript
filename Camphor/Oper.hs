{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.Oper
(Oper(),wrap,unOp
)where
import Camphor.SafePrelude 
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
 