{-# LANGUAGE FlexibleContexts, NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.BF_parser
(BFComm(..)
,parseBF'
)where
import Camphor.SafePrelude 
import Text.Parsec
data BFComm = INCR | DECR | LEFT | RIGHT | INPUT | OUTPUT | WHILE [BFComm] deriving(Show,Eq,Ord)

parseBF' :: Stream s m Char => ParsecT s u m [BFComm]
parseBF' = do{sents <- parseBF;eof;return sents}

parseBF :: Stream s m Char => ParsecT s u m [BFComm]
parseBF = concat <$> many sentences

sentences :: Stream s m Char => ParsecT s u m [BFComm]
sentences = 
 do{char '+'; return [INCR]  } <|> 
 do{char '-'; return [DECR]  } <|> 
 do{char '<'; return [LEFT]  } <|> 
 do{char '>'; return [RIGHT] } <|> 
 do{char ','; return [INPUT] } <|> 
 do{char '.'; return [OUTPUT]} <|> 
 do{char '['; xs <- parseBF; char ']'; return [WHILE xs]} <|> 
 do{noneOf "]"; return []}


 