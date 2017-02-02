{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.PCS_Parser
(parser2'
,Tok(..)
,show'
)where

import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Global
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)

data Tok = 
 CHAR  | DELETE | IDENT Ident   |   NUM Integer   |  
 PAREN | NERAP  | BRACE | ECARB | SCOLON | CNSTNT |
 COMM String    |   OP String   | INFIXL | INFIXR |
 VOID  | CONST  |   SP String                       deriving(Show)

parser2' :: Stream s m Char => ParsecT s u m [Tok]
parser2' = do{ts<-many tok;eof;return ts;}

tok :: Stream s m Char => ParsecT s u m Tok
tok = p_char <|> p_delete <|> p_ident <|> p_num <|> scolon <|>
 paren <|> nerap <|> brace <|> ecarb <|>
 p_comm <|> p_op <|> p_infixl <|> p_infixr <|>
 p_void <|> p_sp <|> p_cnstnt <|> p_const 
 where 
  paren  = char '(' >> return PAREN
  nerap  = char ')' >> return NERAP
  brace  = char '{' >> return BRACE
  ecarb  = char '}' >> return ECARB
  scolon = char ';' >> return SCOLON
 
p_char :: Stream s m Char =>  ParsecT s u m Tok
p_char    = try(do{string "char"    ; notFollowedBy alphaNumBar}) >> return CHAR

p_delete :: Stream s m Char =>  ParsecT s u m Tok
p_delete  = try(do{string "p_delete"  ; notFollowedBy alphaNumBar}) >> return DELETE 

p_infixl :: Stream s m Char =>  ParsecT s u m Tok
p_infixl  = try(do{string "infixl"  ; notFollowedBy alphaNumBar}) >> return INFIXL 

p_infixr :: Stream s m Char =>  ParsecT s u m Tok
p_infixr  = try(do{string "infixr"  ; notFollowedBy alphaNumBar}) >> return INFIXR 

p_void :: Stream s m Char =>  ParsecT s u m Tok
p_void    = try(do{string "p_void"    ; notFollowedBy alphaNumBar}) >> return VOID 

p_cnstnt :: Stream s m Char =>  ParsecT s u m Tok
p_cnstnt  = try(do{string "constant"; notFollowedBy alphaNumBar}) >> return CNSTNT 

p_const :: Stream s m Char =>  ParsecT s u m Tok
p_const   = try(do{string "const"   ; notFollowedBy alphaNumBar}) >> return CONST 

p_ident :: Stream s m Char =>  ParsecT s u m Tok
p_ident   = IDENT <$> identifier

p_num :: Stream s m Char =>  ParsecT s u m Tok
p_num     = NUM   <$> uint' 

p_comm :: Stream s m Char =>  ParsecT s u m Tok
p_comm    = COMM  <$> blockComm 

p_sp :: Stream s m Char =>  ParsecT s u m Tok
p_sp      = SP    <$> many1 space

p_op :: Stream s m Char =>  ParsecT s u m Tok
p_op      = OP    <$> operator


show' :: Tok -> String
show'  CHAR     = "token "     ++show "char"
show'  DELETE   = "token "     ++show "delete"
show' (IDENT i) = "identifier "++show i
show' (NUM n)   = "number "    ++show n
show'  PAREN    = "token "     ++show "("
show'  NERAP    = "token "     ++show ")"
show'  BRACE    = "token "     ++show "{"
show'  ECARB    = "token "     ++show "}"
show'  SCOLON   = "token "     ++show ";"
show' (COMM s)  = "comment "   ++show ("/*"++s++"*/")
show' (OP s)    = "operator "  ++show s
show' INFIXL    = "token "     ++show "infixl"
show' INFIXR    = "token "     ++show "infixr"
show' VOID      = "token "     ++show "void"
show' CONST     = "token "     ++show "const"
show' (SP _)    = "space "     
show' CNSTNT    = "token "     ++show "constant"