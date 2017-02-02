{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.Global
(identifier,identifier'
,nbsp,nbsps,nbnls,newline'
,spaces',spaces1'
,(<++>),(<:>),(>=>),(</>)
,strP
,uint,byte,uint'
,isJust,isNothing
,lib_dir
,Ident,Txt,MemSize,Address
,lift,(<$$>)
,readEith,readMay
)where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Control.Monad
import Text.Parsec hiding(token)
import Data.Char(isSpace,ord)
import Control.Applicative hiding ((<|>),many)
import System.FilePath
import Data.Maybe(isJust,isNothing)

infixr 5 <++>
(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

lib_dir :: FilePath
lib_dir = "lib"



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
strP = fmap (:[])

uint :: Stream s m Char => ParsecT s u m String
uint = many1 digit <?> "unsigned integer"

digit' :: (Stream s m Char,Num a) => ParsecT s u m a
digit' =
 do{char '0';return 0} <|> do{char '1';return 1} <|> do{char '2';return 2} <|>
 do{char '3';return 3} <|> do{char '4';return 4} <|> do{char '5';return 5} <|>
 do{char '6';return 6} <|> do{char '7';return 7} <|> do{char '8';return 8} <|>
 do{char '9';return 9}

uint' :: Stream s m Char => ParsecT s u m Integer
uint' = do{xs <- many1 digit';
 return(foldl(\a b -> 10*a+b)0 xs)
 }<|> try(do{
  char '\'';
  y <- (noneOf "\\\n" <|> parseEsc);
  char '\'';
  return . fromIntegral . ord $ y
  })

byte :: Stream s m Char => ParsecT s u m String
byte = many1 digit <|> 
 try(do{
  char '\'';
  y <- (noneOf "\\\n" <|> parseEsc);
  char '\'';
  return . show . ord $ y
  })

parseEsc :: Stream s m Char => ParsecT s u m Char
parseEsc = do{char '\\';x<-oneOf "'\\abfnrtv\"?0";return(unesc x)}
unesc :: Char -> Char
unesc 'a' = '\a'
unesc 'b' = '\b'
unesc 'f' = '\f'
unesc 'n' = '\n'
unesc 'r' = '\r'
unesc 't' = '\t'
unesc 'v' = '\v'
unesc '0' = '\0'
unesc x   = x -- '\?"
  
newline' :: Stream s m Char => ParsecT s u m ()
newline' = do{newline;return()} <|> do{string "//";many(noneOf "\n");newline;return()} <?> "new line or line comment"
  
type Ident=String
type Txt=String
type MemSize=Integer
type Address=Integer
  
lift :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)
lift = fmap . fmap 

(<$$>) :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)
(<$$>) = lift

readEith :: Read a => e -> String -> Either e a
readEith err s = case [x | (x,t) <- reads s, ("","") <- lex t] of
                [x] -> Right x
                _ -> Left err

readMay :: Read a => String -> Maybe a
readMay s = case [x | (x,t) <- reads s, ("","") <- lex t] of
                [x] -> Just x
                _ -> Nothing