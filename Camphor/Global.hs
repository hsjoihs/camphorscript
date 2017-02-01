{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.Global
(identifier
,identifier'
,nbsp
,nbsps
,(<++>)
,(<:>)
,strP
,uint
,byte
,Ident
,isJust
,Txt
)where

import Text.Parsec hiding(token)
import Data.Char(isSpace,ord)
import Control.Applicative hiding ((<|>),many)

infixr 5 <++>
(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

isJust :: Maybe x -> Bool
isJust (Just _) = True
isJust Nothing = False

identifier :: Stream s m Char => ParsecT s u m Ident
identifier=try((letter<|>char '_') <:> many(alphaNum<|>char '_') )<?>"identifier"

identifier' :: Stream s m Char => ParsecT s u m Ident
identifier'=((letter<|>char '_') <:> many(alphaNum<|>char '_') )<?>"identifier"
 
nbsp :: Stream s m Char => ParsecT s u m Char
nbsp =satisfy (\x->isSpace x && x/='\n')<?>"non-breaking space"

nbsps :: Stream s m Char => ParsecT s u m ()
nbsps=skipMany nbsp

strP :: Stream s m Char => ParsecT s u m Char -> ParsecT s u m String
strP = fmap (\x -> [x])

uint :: Stream s m Char => ParsecT s u m String
uint = many1 digit <?> "unsigned integer"

byte :: Stream s m Char => ParsecT s u m String
byte = many1 digit <|> 
 try(do{
  char '\'';
  y <- (noneOf "\\" <|> do{char '\\';x<-oneOf "'\\";return x});
  char '\'';
  return . show . ord $ y
  })

type Ident=String
type Txt=String