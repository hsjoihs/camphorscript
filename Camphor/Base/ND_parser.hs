{-# LANGUAGE FlexibleContexts ,NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.Base.ND_parser
(parserND'
,ComNum(..)
,Com(..)
,Nul(..)
,ABC(..)
,Chunk
)where

import Camphor.SafePrelude 
import Text.Parsec hiding(token)
import Camphor.Global.Parsers
data ABC a b c = A a | B b | C c deriving(Show)
data ComNum = INC | DEC | MOV | ASR deriving(Show)
data Com    = LOOP | POOL | IN | OUT  deriving(Show)
data Nul    = NUL deriving(Show)
type Chunk  = ABC (ComNum,Integer) Com (Nul,String)

parserND' :: Stream s m Char => ParsecT s u m [Chunk]
parserND' = do{sents <- many sentences;eof;return sents}

sentences :: Stream s m Char => ParsecT s u m Chunk
sentences = (inc <|> dec <|> loop <|> pool <|> mov <|> assert <|> output <|> input <?> "sentence") <|> nul <|> comm 
 where
  uintOr1 = do{spaces;num <- option 1 uint';spaces;char ';';return num}
  uintAndS = do{spaces;num <- uint';spaces;char ';';return num}
  inc    = do{string "inc";num <- uintOr1; return$A(INC,num)}
  dec    = do{string "dec";num <- uintOr1; return$A(DEC,num)}
  mov    = do{string "mov";num <- uintAndS; return$A(MOV,num)}
  assert = do{string "assert_zero";num <- uintAndS; return$A(ASR,num)}
  loop   = do{string "loop";spaces;char ';'; return(B LOOP)}
  pool   = do{string "pool";spaces;char ';'; return(B POOL)}
  input  = do{string "_input";spaces;char ';'; return(B IN)} {- "_input" rather than "input" to avoid 'try' -}
  output = do{string "output";spaces;char ';'; return(B OUT)}
  nul    = do{sp<-many1 space;return$C(NUL,sp)} <?> "space"
  comm   = do{string "/*";comment<-many(noneOf "*");string "*/"<?>"end of block comment";return$C(NUL,"/*"++(comment>>=escape)++"*/")} <?> "comment"
  escape '+' = "_plus_" 
  escape '-' = "_minus_" 
  escape ',' = "_comma_" 
  escape '.' = "_dot_" 
  escape '[' = "{(" 
  escape ']' = ")}" 
  escape '>' = "_gt_" 
  escape '<' = "_lt_" 
  escape  x  = [x]