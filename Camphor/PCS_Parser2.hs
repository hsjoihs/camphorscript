{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.PCS_Parser2
(parser2'
,Tok(..)
,showTok
,tok
,_char , _delete , _paren  , _nerap  , _brace 
 , _ecarb , _scolon , _cnstnt , _infixl , _infixr 
 , _void  , _const, _ident, _num, _comm, _op, _sp, _nl, __
)where

import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Text.Parsec 
import Camphor.PCS_Parser
import Data.Functor.Identity

_nl :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_nl = _sp <|> _comm

__ :: Stream s Identity (SourcePos, Tok) => Parsec s u ()
__ = skipMany _nl

tok :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
tok = _char <|> _delete <|> _paren  <|> _nerap  <|> _brace 
 <|> _ecarb <|> _scolon <|> _cnstnt <|> _infixl <|> _infixr 
 <|> _void  <|> _const <|> _ident <|> _num <|> _comm <|> _op <|> _sp

_char   :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_char   = parseIf CHAR
 
_delete :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_delete = parseIf DELETE
 
_paren  :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_paren  = parseIf PAREN

_nerap  :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_nerap  = parseIf NERAP

_brace  :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_brace  = parseIf BRACE
 
_ecarb  :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_ecarb  = parseIf ECARB 
 
_scolon :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_scolon = parseIf SCOLON

_cnstnt :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_cnstnt = parseIf CNSTNT

_infixl :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_infixl = parseIf INFIXL

_infixr :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_infixr = parseIf INFIXR

_void   :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_void   = parseIf VOID

_const  :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_const  = parseIf CONST

_ident  :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_ident  = parseWhen qqq
 where qqq (_,t@(IDENT _)) = Just t ; qqq(_,_) = Nothing
 
_num   :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_num   = parseWhen qqq
 where qqq (_,t@(NUM   _)) = Just t ; qqq(_,_) = Nothing
 
_comm  :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_comm  = parseWhen qqq
 where qqq (_,t@(NUM   _)) = Just t ; qqq(_,_) = Nothing
 
_op    :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_op    = parseWhen qqq
 where qqq (_,t@(OP    _)) = Just t ; qqq(_,_) = Nothing
 
_sp    :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_sp    = parseWhen qqq
 where qqq (_,t@(SP    _)) = Just t ; qqq(_,_) = Nothing

 
testTokBy :: Eq a => a -> (t, a) -> Maybe a
testTokBy x (_ ,t) =  if x == t then Just t else Nothing

showTok2 :: (a, Tok) -> String
showTok2  = showTok . snd

posFromTok :: (a, b) -> a
posFromTok = fst 

 
parseIf   :: Stream s Identity (SourcePos, Tok) => Tok -> Parsec s u Tok
parseIf x = parseWhen (testTokBy x)

parseWhen :: Stream s Identity (SourcePos, Tok) => ((SourcePos, Tok) -> Maybe Tok) -> Parsec s u Tok
parseWhen = token showTok2 posFromTok