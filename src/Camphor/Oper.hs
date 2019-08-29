{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.Oper
(Oper(),wrap,unOp
)where
import Camphor.SafePrelude 
newtype Oper = Op String deriving(Ord,Show)

delspace :: String -> String
delspace = filter(not . isSpace)


instance Eq Oper where
 (Op str)==(Op str2) = delspace str == delspace str2
 
wrap :: String -> Oper
wrap str = Op (delspace str)

unOp :: Oper -> String
unOp (Op str) = str