{-# OPTIONS -Wall #-}
module Camphor.Global.Synonyms
(Ident,Oper,Txt,MemSize,Address,Fix
)where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)

type Ident=String
type Oper=String
type Txt=String
type MemSize=Integer
type Address=Integer
type Fix = Integer