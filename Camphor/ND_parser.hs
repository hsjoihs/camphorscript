{-# LANGUAGE FlexibleContexts ,NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{- symbolization -}
module Camphor.ND_parser
(parserND,parserND'
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
data Com    =  LOOP | POOL | IN | OUT  deriving(Show)
data Nul    =  NUL deriving(Show)
type Chunk  = ABC (ComNum,Integer) Com (Nul,String)

parserND' :: Stream s m Char => ParsecT s u m [Chunk]
parserND' = do{sents <- many sentences;eof;return sents}

parserND :: Stream s m Char => ParsecT s u m [Chunk]
parserND = many sentences

sentences :: Stream s m Char => ParsecT s u m Chunk
sentences = inc <|> dec <|> loop <|> pool <|> mov <|> assert <|> output <|> input <|> nul <|> comm 
 where
  inc    = do{string "inc";spaces;num<-option 1 uint';spaces;char ';'; return$A(INC,num)}
  dec    = do{string "dec";spaces;num<-option 1 uint';spaces;char ';'; return$A(DEC,num)}
  mov    = do{string "mov";spaces;num<-uint';spaces;char ';'; return$A(MOV,num)}
  assert = do{string "assert_zero";spaces;num<-uint';spaces;char ';'; return$A(ASR,num)}
  loop   = do{string "loop";spaces;char ';'; return(B LOOP)}
  pool   = do{string "pool";spaces;char ';'; return(B POOL)}
  input  = do{string "_input";spaces;char ';'; return(B IN)} {- "_input" rather than "input" to avoid 'try' -}
  output = do{string "output";spaces;char ';'; return(B OUT)}
  nul    = do{sp<-many1 space;return$C(NUL,sp)}
  comm   = do{string "/*";comment<-many(noneOf "*");string "*/";return$C(NUL,"/*"++(comment>>=escape)++"*/")}
  escape '+' = "_plus_" 
  escape '-' = "_minus_" 
  escape ',' = "_comma_" 
  escape '.' = "_dot_" 
  escape '[' = "{(" 
  escape ']' = ")}" 
  escape '>' = "_gt_" 
  escape '<' = "_lt_" 
  escape  x  = [x]