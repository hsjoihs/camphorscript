{-# OPTIONS -Wall #-}
module Camphor.Global.Synonyms
(Ident,Txt,MemSize,Address,Fix,Oper(),wrap,unOp,PragmaData,Options,Includers,FileToTxt
)where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Oper
import qualified Data.Map as M

type Ident = String
type Txt = String
type MemSize = Integer
type Address = Integer
type Fix = Integer
type PragmaData = [String]
type Options = [String]
type FileToTxt = M.Map FilePath (FilePath,Txt)
type Includers = (FileToTxt,FileToTxt,M.Map Ident String)