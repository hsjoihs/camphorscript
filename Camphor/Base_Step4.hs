{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind#-}
{- translation -}
module Camphor.Base_Step4
(parser4
,Com4_bot(..)
,Com4_top(..)
,Com4_mid(..)
,OneOf(..)
,sentences_
,Tree(..)
,convert4
,convert4'
,step4
) where


import Camphor.Global
import Text.Parsec hiding(token)
import Text.Parsec.Error
import Text.Parsec.Pos
import Control.Applicative hiding ((<|>),many)
import qualified Data.Map as M
import Camphor.NonNullList

comment_4_start :: String
comment_4_start = "/*"

comment_4_end :: String
comment_4_end = "*/"

data Com4_top=DEF|DEL|REA|WRI|COM|NUL deriving(Show)
data Com4_mid=ADD|SUB deriving(Show)
data Com4_bot=WHI deriving(Show)

data OneOf a b c = Null | Top a | Mid b | Bot c deriving (Show)

data Tree b c d f = Ns [OneOf d f(b,c,Tree b c d f)] deriving(Show)
type Node b c d f =        OneOf d f(b,c,Tree b c d f)

type Set4 = Node Com4_bot Ident (Com4_top,String) (Com4_mid,[Char],[Char])

parser4 :: Stream s m Char => ParsecT s u m [Set4]
parser4 = many sentences_

parser4TillLast :: Stream s m Char => ParsecT s u m [Set4]
parser4TillLast = do{sents <- parser4; eof; return sents}


sentences_:: Stream s m Char => ParsecT s u m Set4

sentences_=def<|>del<|>add<|>sub<|>while<|>read_<|>write<|>nul<|>emp<|>comm 
 where
  def  =try(do{string"char"  ;space ;spaces;xs<-identifier;spaces; char ';';return$Top(DEF,xs)})
  del  =try(do{string"delete";space ;spaces;xs<-identifier;spaces; char ';';return$Top(DEL,xs)})
  add  =try(do{xs<-identifier;spaces;char '+';spaces;char '=';spaces; ys<-byte;spaces;char ';';return$Mid(ADD,xs,ys)})
  sub  =try(do{xs<-identifier;spaces;char '-';spaces;char '=';spaces; ys<-byte;spaces;char ';';return$Mid(SUB,xs,ys)})
  while=try(do{string "while";spaces;char '(';spaces;xs<-identifier;spaces;char ')';spaces;char '{';spaces;ks<-parser4;spaces;char '}';return$Bot(WHI,xs,Ns ks)})
  read_=try(do{string "read" ;spaces;char '(';spaces;xs<-identifier;spaces;char ')';spaces;char ';';return$Top(REA,xs)})
  write=try(do{string "write";spaces;char '(';spaces;xs<-identifier;spaces;char ')';spaces;char ';';return$Top(WRI,xs)})
  nul  =try(do{sp<-many1 space;return$Top(NUL,sp)})
  emp  =    do{char ';';return Null}
  comm =try(do{string comment_4_start;xs<-many(noneOf "*");string comment_4_end;return$Top(COM,comment_4_start++xs++comment_4_end)})


type VarNum = Integer
type Table4 = M.Map Ident VarNum -- variable, variable num
type CurrState = (Int,NonEmpty Table4,[VarNum]) -- block num, defined variables(inner scope first), used variable num
{- Table4 must not be empty -}

step4 :: FilePath -> Txt -> Either ParseError Txt
step4 file str = do{sets<-parse parser4TillLast (file++"--step4") str;convert4 file sets}

convert4 :: FilePath -> [Set4] -> Either ParseError Txt
convert4 file xs = convert4' file ((1,M.empty :| [],[]),xs)

minUnused :: [VarNum] -> VarNum
minUnused xs = head$filter(`notElem` xs) [0..]

remove :: VarNum->[VarNum]->[VarNum]
remove x xs = filter (/=x) xs

lookup'::Ord k=>k->[M.Map k a]->Maybe a
lookup' _ []     = Nothing
lookup' i (t:ts) = case(M.lookup i t)of Just a->Just a; Nothing-> lookup' i ts

convert4' :: FilePath -> (CurrState,[Set4]) -> Either ParseError String


convert4' _( _            ,[]                    ) = Right ""


convert4' f((n ,s:|st ,ls),(Top(DEF,ide     ):xs)) 
 | isJust(M.lookup ide s)                        = Left $newErrorMessage (Message$"identifier "++show ide++"is already defined")(newPos (f++"--step4'") 0 0)
 | otherwise                                     = convert4' f((n, M.insert ide new s :| st,new:ls),xs)
  where new　=　minUnused ls

convert4' f((n ,s:|st ,ls),(Top(DEL,ide     ):xs)) = case (M.lookup ide s) of
   Just  k                                      -> convert4' f((n, M.delete ide s :| st,remove k ls),xs)
   Nothing                                      -> Left $newErrorMessage (Message$"identifier "++show ide++"is not defined or is already deleted in this scope")(newPos "step4" 0 0)

convert4' f(state         ,(Top(NUL,sp      ):xs)) = (sp++)<$>convert4' f(state,xs) 


convert4' f((n ,st    ,ls),(Mid(ADD,ide,  nm):xs)) = case (lookup' ide (toList st)) of
   Just  k                                      -> (\x->"mov "++show k++"; inc "++nm++"; "++x)<$>convert4'  f((n,st,ls),xs)
   Nothing                                      -> Left $newErrorMessage (Message$"identifier "++show ide++"is not defined")(newPos (f++"--step4'") 0 0)
   
convert4' f((n ,st    ,ls),(Mid(SUB,ide,  nm):xs)) = case (lookup' ide (toList st)) of
   Just  k                                      -> (\x->"mov "++show k++"; dec "++nm++"; "++x)<$>convert4'  f((n,st,ls),xs)
   Nothing                                      -> Left $newErrorMessage (Message$"identifier "++show ide++"is not defined")(newPos (f++"--step4'") 0 0)

convert4' f((n ,st    ,ls),(Top(REA,ide     ):xs)) = case (lookup' ide (toList st)) of
   Just  k                                      -> (\x->"mov "++show k++"; _input; "++x)<$>convert4'  f((n,st,ls),xs)
   Nothing                                      -> Left $newErrorMessage (Message$"identifier "++show ide++"is not defined")(newPos (f++"--step4'") 0 0)

convert4' f((n ,st    ,ls),(Top(WRI,ide     ):xs)) = case (lookup' ide (toList st)) of
   Just  k                                      -> (\x->"mov "++show k++"; output; "++x)<$>convert4'  f((n,st,ls),xs)
   Nothing                                      -> Left $newErrorMessage (Message$"identifier "++show ide++"is not defined")(newPos (f++"--step4'") 0 0)

convert4' f(state         ,(Null             :xs)) = (' ':)<$>convert4' f(state,xs)
convert4' f(state         ,(Top(COM,cm)      :xs)) = (cm++)<$>convert4' f(state,xs)

convert4' f((n ,st    ,ls),(Bot(WHI,ide,Ns v):xs)) = case (lookup' ide (toList st)) of
   Just k                                       -> do
    res1 <- convert4'  f((n+1,M.empty `cons` st,ls),v )
    res2 <- convert4'  f((n  ,st               ,ls),xs)
    return $ "mov " ++ show k ++ "; loop; " ++ res1 ++ "mov " ++ show k ++ "; pool; " ++ res2
   Nothing                                      -> Left $newErrorMessage (Message$"identifier "++show ide++"is not defined")(newPos (f++"--step4'") 0 0)