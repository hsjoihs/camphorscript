{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind -fno-warn-unused-imports #-}
module CamphorR.R_Base_Step4
(step4_R


)where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error)
import Camphor.Global
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)
import Text.Parsec.Error
import Text.Parsec.Pos
import Camphor.ND_parser
import qualified Data.Set as S

-- type Chunk = Either (ComNum,Integer) (Com7,String)

step4_R :: String -> Either ParseError String
step4_R str= do
 parsed <- parse parserND' ("Rstep4") str
 convert4_R parsed

convert4_R :: [Chunk] -> Either ParseError String
convert4_R cs = ((header cs++"\n")++) <$> convert4_R' (0,S.empty) cs

getVarName :: Address -> Ident
getVarName = ("v_" ++) . show 

type Stat = (Address,S.Set Address)

convert4_R' :: Stat -> [Chunk] -> Either ParseError String
convert4_R' _      []               = Right ""
convert4_R' (v,set)(A(INC ,num):cs) = ((getVarName v++"+="++show num++";")++) <$> convert4_R' (v,set) cs
convert4_R' (v,set)(A(DEC ,num):cs) = ((getVarName v++"-="++show num++";")++) <$> convert4_R' (v,set) cs
convert4_R' (_,set)(A(MOV ,num):cs) =                                             convert4_R' (num,set) cs
convert4_R' (v,set)(A(ASR ,_  ):cs) =                                             convert4_R' (v,set) cs 
convert4_R' (v,set)(B LOOP     :cs) = (("while(" ++getVarName v++"){")++)     <$> convert4_R' (v,set) cs
convert4_R' (v,set)(B POOL     :cs) = ("}"++)                                 <$> convert4_R' (v,set) cs
convert4_R' (v,set)(B IN       :cs) = (("read("  ++getVarName v++ ");")++)    <$> convert4_R' (v,set) cs
convert4_R' (v,set)(B OUT      :cs) = (("write(" ++getVarName v++ ");")++)    <$> convert4_R' (v,set) cs
convert4_R' (v,set)(C(NUL ,sp ):cs) 
 | all (`elem` " \t\n\r") sp        = (sp++)                                  <$> convert4_R' (v,set) cs
 | otherwise                        = (("/*" ++sp++ "*/")++)                  <$> convert4_R' (v,set) cs
 
header :: [Chunk] -> String
header cs = (\x -> "char "++getVarName x++";") =<< (S.toList . S.fromList) (0:[ num | A(MOV,num) <- cs])