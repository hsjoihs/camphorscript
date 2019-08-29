{-# LANGUAGE FlexibleContexts ,NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.Base.NL_parser
(parserNL'
,Chunk(..)
)where

import Camphor.SafePrelude 
import Text.Parsec hiding(token)
import Camphor.Global.Parsers
data Chunk  = INC Integer | DEC Integer | MOV Integer | ASR Integer | LOOP | POOL | IN | OUT | NUL String deriving(Show)

parserNL' :: Stream s m Char => ParsecT s u m [Chunk]
parserNL' = do{sents <- many sentences;eof;return sents}

sentences :: Stream s m Char => ParsecT s u m Chunk
sentences = inc <|> dec <|> loop <|> pool <|> mov <|> assert <|> assert2 <|> assert3 <|> output <|> input <|> nul <|> comm 
 where
  uintOr1 = do{spaces;num <- option 1 uint';spaces;char ';';return num}
  uintAndS = do{spaces;num <- uint';spaces;char ';';return num}
  inc    = do{string "inc";num <- uintOr1; return$INC num}
  dec    = do{string "dec";num <- uintOr1; return$DEC num}
  mov    = do{string "mov";num <- uintAndS; return$MOV num}
  assert = do{string "unsafe_assert";num <- uintAndS; return$ASR num}
  assert2= do{string "begin_static_assert";num <- uintAndS; return$ASR num}
  assert3= do{string "end_static_assert";num <- uintAndS; return$ASR num}
  loop   = do{string "loop";spaces;char ';'; return LOOP}
  pool   = do{string "pool";spaces;char ';'; return POOL}
  input  = do{string "_input";spaces;char ';'; return IN} {- "_input" rather than "input" to avoid 'try' -}
  output = do{string "output";spaces;char ';'; return OUT}
  nul    = do{sp <- many1 space;return$NUL sp}
  comm   = do{string "/*";comment <- many(noneOf "*");string "*/";return$NUL$"/*"++(comment>>=escape)++"*/"}
  escape '+' = "_plus_" 
  escape '-' = "_minus_" 
  escape ',' = "_comma_" 
  escape '.' = "_dot_" 
  escape '[' = "{(" 
  escape ']' = ")}" 
  escape '>' = "_gt_" 
  escape '<' = "_lt_" 
  escape  x  = [x]