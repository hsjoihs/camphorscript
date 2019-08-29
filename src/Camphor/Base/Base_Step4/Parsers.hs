{-# LANGUAGE FlexibleContexts ,NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{- Desugaring -}
module Camphor.Base.Base_Step4.Parsers
(parser4'
) where
import Camphor.SafePrelude 
import Camphor.Global.Parsers
import Camphor.Base.Base_Step4.Simplify
import Camphor.Base.Base_Step4.IdentFactory
import Text.Parsec hiding(token)

parser4' :: Stream s m Char => ParsecT s u m [Set4]
parser4' = parser4 <* eof

parId :: Stream s m Char => ParsecT s u m Ident2
parId = do{spaces';char '(';spaces';xs <- parseId;spaces';char ')';spaces';return xs}

parIdSC :: Stream s m Char => ParsecT s u m Ident2
parIdSC = parId <* char ';'

parser4 :: Stream s m Char => ParsecT s u m [Set4]
parser4 = concat <$> many sentences4


sentences4 :: Stream s m Char => ParsecT s u m [Set4]
sentences4 = def <|> del <|> asser <|> add <|> sub <|> while <|> block <|> read_ <|> write <|> nul <|> emp <|> comm 
 where 
  idSC  = do{xs <- parseId;spaces'; char ';';return xs}
  eqIdSC= do{spaces';char '=';spaces'; ys <- uint';spaces';char ';';return ys}
  def   = do{try(do{string "char"  ;spaces';});xs <- idSC;return[DEF xs]}
  del   = do{try(do{string "delete";spaces';});xs <- idSC;return[DEL xs]}
  asser = do{try(do{string "assert_zero";spaces';});xs <- idSC;return[AS0 xs]}
  add   = try(do{xs <- parseId ;spaces';char '+';ys <- eqIdSC;return[ADD xs   ys ]})
  sub   = try(do{xs <- parseId ;spaces';char '-';ys <- eqIdSC;return[ADD xs (-ys)]})
  read_ = try(do{string "read"  ;xs <- parIdSC;return[REA xs]})
  write = try(do{string "write" ;xs <- parIdSC;return[WRI xs]})
  nul   = try(do{sp <- many1 space;return[NUL sp]})
  emp   =     do{char ';';return[Null]}
  comm  = try(do{string "/*";xs<-many(noneOf "*");string "*/";return[COM("/*"++xs++"*/")]})

while :: Stream s m Char => ParsecT s u m [Set4] 
while = try(do{
 string "while"; xs <- parId;
 spaces'; ks <- curly;
 return[WHI xs(Ns ks)]})
 
block :: Stream s m Char => ParsecT s u m [Set4]
block = try(do{
 ks <- curly;
 return[BLO (Ns ks)]})

curly :: Stream s m Char => ParsecT s u m [Set4]
curly = do{char '{';
 spaces'; ks <- parser4; spaces';
 char '}';return ks}

