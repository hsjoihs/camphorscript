{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  -fno-warn-missing-signatures -fno-warn-unused-imports #-}
{- translation -}
module Camphor.Base_Step4
(parser4
,Com4(..)
,sentences_,def,del,add,sub,while,read_,write,nul,emp,comm
,Tree
) where


import Camphor.Global
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)

data Com4=DEF|DEL|ADD|SUB|WHI|REA|NUL|EMP|COM deriving(Show)

data Tree a b c = Node a | Nodes [(b,c,Tree a b c)] deriving(Show)

parser4=many sentences_


sentences_:: Stream s m Char =>ParsecT s u m (Com4,Ident,Tree [Char] Com4 [Char])
sentences_=def<|>del<|>add<|>sub<|>while<|>read_<|>write<|>nul<|>emp<|>comm
def=try(do{string"char"  ; space ; spaces; xs<-identifier; spaces; char ';';return (DEF,xs,Node "")})
del=try(do{string"delete"; space ; spaces; xs<-identifier; spaces; char ';';return (DEL,xs,Node "")})
add=try(do{xs<-identifier; spaces; char '+'; spaces; char '='; spaces; ys<-uint;spaces;char ';';return (ADD,xs,Node ys)})
sub=try(do{xs<-identifier; spaces; char '-'; spaces; char '='; spaces; ys<-uint;spaces;char ';';return (SUB,xs,Node ys)})
  
while=try(
 do{
  string "while";
  spaces;
  char '(';spaces;xs<-identifier;spaces;char ')';
  spaces;
  char '{'; 
  spaces; 
  ks<-parser4;
  spaces;
  char '}';
  return (WHI,xs,Nodes ks)
  }
  )
  
read_=try(
 do{
  string "read";
  spaces;
  char '(';spaces;xs<-identifier;spaces;char ')';
  spaces;
  char ';';
  return (REA,xs,Node "")
  }
  )
  
write=try(
 do{
  string "write";
  spaces;
  char '(';spaces;xs<-identifier;spaces;char ')';
  spaces;
  char ';';
  return (REA,xs,Node "")
  }
  )
nul=try(do{sp<-many1 space;return (NUL,"",Node sp)})

emp=do{char ';';return(EMP,"",Node "")}
comm=try(do{string "/*";xs<-many(noneOf "*");string "*/";return(COM,"",Node("/*"++xs++"*/"))})
  
