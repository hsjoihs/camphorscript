{-# LANGUAGE FlexibleContexts ,NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.Base.Base_Step2.PCS_Parser2
(Tok(..)
,showTok2
--,tok
,_char,_delete,_paren,_nerap,_brace
,_ecarb,_scolon,_cnstnt,_infixl,_infixr
,_void,_const,_ident,_num,_pragma,_comm,_op,_sp,_nl,__
,_and,_eq,_zero,_comma,_syntax,_BLOCK

)where

import Camphor.SafePrelude
import Text.Parsec 
import Camphor.Base.Base_Step2.Type
import Camphor.Base.Base_Step2.PCS_Parser
import Camphor.Global.Synonyms


_nl :: Stream s Identity (SourcePos, Tok) => Parsec s u String
_nl = _sp <|> _comm

__ :: Stream s Identity (SourcePos, Tok) => Parsec s u ()
__ = skipMany _nl

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

_syntax :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_syntax = parseIf SYNTAX

_infixl :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_infixl = parseIf INFIXL

_infixr :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_infixr = parseIf INFIXR

_void   :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_void   = parseIf VOID

_BLOCK   :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_BLOCK   = parseIf BLOCK

_const  :: Stream s Identity (SourcePos, Tok) => Parsec s u Tok
_const  = parseIf CONST

_ident  :: Stream s Identity (SourcePos, Tok) => Parsec s u Ident2
_ident  = parseWhen qqq
 where qqq (_,IDENT t) = Just t ; qqq(_,_) = Nothing
 
_num   :: Stream s Identity (SourcePos, Tok) => Parsec s u Integer
_num   = parseWhen qqq
 where qqq (_,NUM   t) = Just t ; qqq(_,_) = Nothing
 
_comm  :: Stream s Identity (SourcePos, Tok) => Parsec s u String
_comm  = parseWhen qqq
 where qqq (_,COMM  t) = Just t ; qqq(_,_) = Nothing
 
_pragma :: Stream s Identity (SourcePos, Tok) => Parsec s u PragmaData
_pragma  = parseWhen qqq 
 where qqq (_,PRAGMA t) = Just t ; qqq(_,_) = Nothing
 
_op    :: Stream s Identity (SourcePos, Tok) => Parsec s u Oper
_op    = parseWhen qqq
 where qqq (_,OP    t) = Just t ; qqq(_,_) = Nothing
 
 
_sp    :: Stream s Identity (SourcePos, Tok) => Parsec s u String
_sp    = parseWhen qqq
 where qqq (_,SP    t) = Just t ; qqq(_,_) = Nothing

 
testTokBy :: Eq a => a -> (t, a) -> Maybe a
testTokBy x (_ ,t) =  if x == t then Just t else Nothing

showTok2 :: (a, Tok) -> String
showTok2  = showTok . snd

posFromTok :: (a, b) -> a
posFromTok = fst 

 
parseIf   :: Stream s Identity (SourcePos, Tok) => Tok -> Parsec s u Tok
parseIf x = parseWhen (testTokBy x)

parseWhen :: Stream s Identity (SourcePos, Tok) => ((SourcePos, Tok) -> Maybe a) -> Parsec s u a
parseWhen = token showTok2 posFromTok

_and :: Stream s Identity (SourcePos, Tok) => Parsec s u () 
_and = parseWhen qqq
 where
  qqq(_,OP o) = if unOp o == "&" then Just () else Nothing
  qqq(_,_   ) = Nothing
  
_eq :: Stream s Identity (SourcePos, Tok) => Parsec s u ()
_eq = parseWhen qqq
 where
  qqq(_,OP o) = if unOp o == "=" then Just () else Nothing
  qqq(_,_   ) = Nothing
  
_comma :: Stream s Identity (SourcePos, Tok) => Parsec s u ()
_comma = parseWhen qqq
 where
  qqq(_,OP o) = if unOp o == "," then Just () else Nothing
  qqq(_,_   ) = Nothing
  
_zero :: Stream s Identity (SourcePos, Tok) => Parsec s u ()
_zero = parseWhen qqq
 where 
  qqq(_,NUM t) = if t == 0 then Just () else Nothing 
  qqq(_,_    ) = Nothing