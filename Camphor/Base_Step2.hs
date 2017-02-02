{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind   -fno-warn-unused-imports -fno-warn-missing-signatures #-}
{- Functional macro expansion -}
module Camphor.Base_Step2
(step2
,parser2'

) where

import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error{-,undefined-})
import Camphor.Global
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)

data Tok = 
 CHAR  | DELETE | IDENT String  |   NUM Integer   | SCOLON | 
 PAREN | NERAP  | BRACE | ECARB |
 COMM String    |   OP String   | INFIXL | INFIXR |
 SP String      | VOID  | CONST | CNSTNT | CHRREF   deriving(Show)

step2 :: FilePath -> Txt -> Either ParseError Txt
step2 file str = do
 ts <- parse parser2' (file ++ "-step2") str
 convert2 file ts
 
parser2' :: Stream s m Char => ParsecT s u m [Tok]
parser2' = do{ts<-many tok;eof;return ts;}
{-
parser2 :: Stream s m Char => ParsecT s u m [Tok]
parser2 = many tok
-}
tok :: Stream s m Char => ParsecT s u m Tok
tok = _char <|> delete <|> ident <|> num <|> scolon <|>
 paren <|> nerap <|> brace <|> ecarb <|>
 comm <|> op <|> _infixl <|> _infixr <|>
 void <|> sp <|> cnstnt <|> _const 
 where 
  paren  = char '(' >> return PAREN
  nerap  = char ')' >> return NERAP
  brace  = char '{' >> return BRACE
  ecarb  = char '}' >> return ECARB
  scolon = char ';' >> return SCOLON
 
_char   = try(do{string "char"    ; notFollowedBy alphaNumBar}) >> return CHAR
delete  = try(do{string "delete"  ; notFollowedBy alphaNumBar}) >> return DELETE 
_infixl = try(do{string "infixl"  ; notFollowedBy alphaNumBar}) >> return INFIXL 
_infixr = try(do{string "infixr"  ; notFollowedBy alphaNumBar}) >> return INFIXR 
void    = try(do{string "void"    ; notFollowedBy alphaNumBar}) >> return VOID 
cnstnt  = try(do{string "constant"; notFollowedBy alphaNumBar}) >> return CNSTNT 
_const  = try(do{string "const"   ; notFollowedBy alphaNumBar}) >> return CONST 
ident   = IDENT <$> identifier
num     = NUM   <$> uint' 
comm    = COMM  <$> blockComm 
sp      = SP    <$> many1 space
op      = OP    <$> operator

 
convert2 :: FilePath -> [Tok] -> Either ParseError Txt
convert2 file ts = undefined -- Right(show ts) {- for debug -}