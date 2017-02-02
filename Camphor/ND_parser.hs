{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{- symbolization -}
module Camphor.ND_parser
(parserND,parserND'
,ComNum(..)
,Com7(..)
,Chunk
)where

import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Text.Parsec hiding(token)
import Camphor.Global

data ComNum = INC | DEC | MOV
data Com7 =  LOOP | POOL | IN | OUT | NUL deriving(Show)
type Chunk = Either (ComNum,Integer) (Com7,String)

parserND' :: Stream s m Char => ParsecT s u m [Chunk]
parserND' = do{sents<-many sentences;eof;return sents}

parserND :: Stream s m Char => ParsecT s u m [Chunk]
parserND = many sentences

sentences :: Stream s m Char => ParsecT s u m Chunk
sentences = inc <|> dec <|> loop <|> pool <|> mov <|> assert <|> output <|> input <|> nul <|> comm 
 where
  inc    = do{string "inc";spaces;num<-option 1 uint';spaces;char ';'; return$Left(INC,num)}
  dec    = do{string "dec";spaces;num<-option 1 uint';spaces;char ';'; return$Left(DEC,num)}
  mov    = do{string "mov";spaces;num<-uint';spaces;char ';'; return$Left(MOV,num)}
  assert = do{string "assert_zero";spaces;_<-uint';spaces;char ';'; return$Right(NUL,"")}
  loop   = do{string "loop";spaces;char ';'; return$Right(LOOP,"")}
  pool   = do{string "pool";spaces;char ';'; return$Right(POOL,"")}
  input  = do{string "_input";spaces;char ';'; return$Right(IN,"")} {- "_input" rather than "input" to avoid 'try' -}
  output = do{string "output";spaces;char ';'; return$Right(OUT,"")}
  nul    = do{sp<-many1 space;return$Right(NUL,sp)}
  comm   = do{string "/*";comment<-many(noneOf "*");string "*/";return$Right(NUL,"/*"++(comment>>=escape)++"*/")}
  escape '+' = "_plus_" 
  escape '-' = "_minus_" 
  escape ',' = "_comma_" 
  escape '.' = "_dot_" 
  escape '[' = "{(" 
  escape ']' = ")}" 
  escape '>' = "_gt_" 
  escape '<' = "_lt_" 
  escape  x  = [x]