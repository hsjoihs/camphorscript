{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
{- translation -}
module Camphor.Base_Step4_1
(parser4
,Com4(..)
,sentences_
,Tree(..)
,Set4
) where


import Camphor.Global
import Text.Parsec hiding(token)

data Com4=DEF|DEL|ADD|SUB|WHI|REA|WRI|NUL|EMP|COM deriving(Show)

data Tree a b c = None | Node a | Nodes [(b,c,Tree a b c)] deriving(Show)

type Set4=(Com4,Ident,Tree [Char] Com4 [Char])

parser4:: Stream s m Char =>ParsecT s u m [Set4]
parser4=many sentences_


sentences_:: Stream s m Char =>ParsecT s u m Set4

sentences_=def<|>del<|>add<|>sub<|>while<|>read_<|>write<|>nul<|>emp<|>comm 
 where
  def  =try(do{string"char"  ;space ;spaces;xs<-identifier;spaces; char ';';return (DEF,xs,None)})
  del  =try(do{string"delete";space ;spaces;xs<-identifier;spaces; char ';';return (DEL,xs,None)})
  add  =try(do{xs<-identifier;spaces;char '+';spaces;char '=';spaces; ys<-uint;spaces;char ';';return (ADD,xs,Node ys)})
  sub  =try(do{xs<-identifier;spaces;char '-';spaces;char '=';spaces; ys<-uint;spaces;char ';';return (SUB,xs,Node ys)})
  while=try(do{string "while";spaces;char '(';spaces;xs<-identifier;spaces;char ')';spaces;char '{';spaces;ks<-parser4;spaces;char '}';return(WHI,xs,Nodes ks)})
  read_=try(do{string "read" ;spaces;char '(';spaces;xs<-identifier;spaces;char ')';spaces;char ';';return (REA,xs,None)})
  write=try(do{string "write";spaces;char '(';spaces;xs<-identifier;spaces;char ')';spaces;char ';';return (WRI,xs,None)})
  nul=try(do{sp<-many1 space;return (NUL,"",Node sp)})
  emp=do{char ';';return(EMP,"",None)}
  comm=try(do{string "/*";xs<-many(noneOf "*");string "*/";return(COM,"",Node("/*"++xs++"*/"))})