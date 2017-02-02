{-# OPTIONS -Wall #-}
module Camphor.Global.Synonyms
(Ident
,Txt,MemSize,Address,Fix,Oper(),wrap,unOp
)where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Oper
type Ident=String
type Txt=String
type MemSize=Integer
type Address=Integer
type Fix = Integer