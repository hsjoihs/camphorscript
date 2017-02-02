{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.PCS_Parser
(parser2'
,Tok(..)
,show'
,_char,_delete,_ident,_num,_scolon,_paren , _nerap , _brace , _ecarb ,
 _comm , _op , _infixl , _infixr ,
 _void , _sp , _cnstnt , _const ,__,_nl
)where

import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Global
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)


parser2' :: Stream s m Char => ParsecT s u m [Tok]
parser2' = do{ts<-many tok;eof;return ts;}

data Tok = 
 CHAR  | DELETE | IDENT Ident   |   NUM Integer   |  
 PAREN | NERAP  | BRACE | ECARB | SCOLON | CNSTNT |
 COMM String    |   OP String   | INFIXL | INFIXR |
 VOID  | CONST  |   SP String                       deriving(Show)



tok :: Stream s m Char => ParsecT s u m Tok
tok = _char <|> _delete <|> _ident <|> _num <|> _scolon <|>
 _paren <|> _nerap <|> _brace <|> _ecarb <|>
 _comm <|> _op <|> _infixl <|> _infixr <|>
 _void <|> _sp <|> _cnstnt <|> _const 

_paren :: Stream s m Char =>  ParsecT s u m Tok
_paren  = char '(' >> return PAREN
_nerap :: Stream s m Char =>  ParsecT s u m Tok
_nerap  = char ')' >> return NERAP
_brace :: Stream s m Char =>  ParsecT s u m Tok
_brace  = char '{' >> return BRACE
_ecarb :: Stream s m Char =>  ParsecT s u m Tok
_ecarb  = char '}' >> return ECARB
_scolon :: Stream s m Char =>  ParsecT s u m Tok
_scolon = char ';' >> return SCOLON



__ :: Stream s m Char => ParsecT s u m () 
__ = skipMany _nl

_nl :: Stream s m Char =>  ParsecT s u m Tok
_nl = _sp <|> _comm

 
_char :: Stream s m Char =>  ParsecT s u m Tok
_char    = try(do{string "char"    ; notFollowedBy alphaNumBar}) >> return CHAR

_delete :: Stream s m Char =>  ParsecT s u m Tok
_delete  = try(do{string "_delete"  ; notFollowedBy alphaNumBar}) >> return DELETE 

_infixl :: Stream s m Char =>  ParsecT s u m Tok
_infixl  = try(do{string "infixl"  ; notFollowedBy alphaNumBar}) >> return INFIXL 

_infixr :: Stream s m Char =>  ParsecT s u m Tok
_infixr  = try(do{string "infixr"  ; notFollowedBy alphaNumBar}) >> return INFIXR 

_void :: Stream s m Char =>  ParsecT s u m Tok
_void    = try(do{string "_void"    ; notFollowedBy alphaNumBar}) >> return VOID 

_cnstnt :: Stream s m Char =>  ParsecT s u m Tok
_cnstnt  = try(do{string "constant"; notFollowedBy alphaNumBar}) >> return CNSTNT 

_const :: Stream s m Char =>  ParsecT s u m Tok
_const   = try(do{string "const"   ; notFollowedBy alphaNumBar}) >> return CONST 

_ident :: Stream s m Char =>  ParsecT s u m Tok
_ident   = IDENT <$> identifier

_num :: Stream s m Char =>  ParsecT s u m Tok
_num     = NUM   <$> uint' 

_comm :: Stream s m Char =>  ParsecT s u m Tok
_comm    = COMM  <$> blockComm 

_sp :: Stream s m Char =>  ParsecT s u m Tok
_sp      = SP    <$> many1 space

_op :: Stream s m Char =>  ParsecT s u m Tok
_op      = OP    <$> operator


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