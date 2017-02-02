{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.Global
(identifier,identifier'
,nbsp,nbsps,nbnls,newline'
,spaces',spaces1'
,(<++>),(<:>),(>=>),(</>)
,strP
,uint,byte
,isJust
,lib_dir
,Ident,Txt
)where

import Control.Monad
import Text.Parsec hiding(token)
import Data.Char(isSpace,ord)
import Control.Applicative hiding ((<|>),many)
import System.FilePath

infixr 5 <++>
(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

lib_dir :: FilePath
lib_dir = "lib"

isJust :: Maybe x -> Bool
isJust (Just _) = True
isJust Nothing = False

identifier :: Stream s m Char => ParsecT s u m Ident
identifier=try((letter <|> char '_') <:> many(alphaNum <|> char '_') )<?>"identifier"

identifier' :: Stream s m Char => ParsecT s u m Ident
identifier' = ((letter <|> char '_') <:> many(alphaNum <|> char '_') )<?>"identifier"
 
nbsp :: Stream s m Char => ParsecT s u m Char
nbsp = satisfy (\x->isSpace x && x/='\n')<?>"non-breaking space"

nbsps :: Stream s m Char => ParsecT s u m ()
nbsps = skipMany nbsp

spaces' :: Stream s m Char => ParsecT s u m ()
spaces' = skipMany space'

spaces1' :: Stream s m Char => ParsecT s u m ()
spaces1' = skipMany1 space'

space' :: Stream s m Char => ParsecT s u m Char
space' = 
  space <|> 
  do{string "/*"; manyTill anyChar(try(string "*/"));return ' ';} <|> 
  do{string "//";many(noneOf "\n");newline;return ' '} <?> "space, block comment or line comment"



-- non-breaking space or comment
nbnls :: Stream s m Char => ParsecT s u m ()
nbnls=skipMany nbnl
 where 
  nbnl = nbsp <|> try(do{string "/*"; manyTill anyChar(try(string "*/"));return ' ';}) <?> "non-breaking space or block comment"


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

newline' :: Stream s m Char => ParsecT s u m ()
newline' = do{newline;return()} <|> do{string "//";many(noneOf "\n");newline;return()} <?> "new line or line comment"
  
type Ident=String
type Txt=String