{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.PCS_Parser
(parser2'
,Tok(..)
,showTok

)where

import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Global.Parsers
import Camphor.Global.Synonyms
import Text.Parsec hiding(token)

parser2' :: Stream s m Char => ParsecT s u m [(SourcePos,Tok)]
parser2' = do{ts<-many tok;eof;return ts;}

data Tok = 
 CHAR  | DELETE | IDENT Ident   |   NUM Integer   |  
 PAREN | NERAP  | BRACE | ECARB | SCOLON | CNSTNT |
 COMM String    |    OP Oper    | INFIXL | INFIXR |
 VOID  | CONST  |   SP String                       deriving(Show,Eq)



tok :: Stream s m Char => ParsecT s u m (SourcePos,Tok)
tok = _char <|> _delete  <|> _num <|> _scolon <|>
 _paren <|> _nerap <|> _brace <|> _ecarb <|>
 _comm <|> _op <|> -- _comm first, _op second
 _infixl <|> _infixr <|>
 _void <|> _sp <|> _cnstnt <|> _const <|> _ident

_paren :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_paren  = do{p <- getPosition; char '('; return (p,PAREN)}
_nerap :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_nerap  = do{p <- getPosition; char ')'; return (p,NERAP)}
_brace :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_brace  = do{p <- getPosition; char '{'; return (p,BRACE)}
_ecarb :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_ecarb  = do{p <- getPosition; char '}'; return (p,ECARB)}
_scolon :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_scolon = do{p <- getPosition; char ';'; return (p,SCOLON)}



__ :: Stream s m Char => ParsecT s u m () 
__ = skipMany _nl

_nl :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_nl = _sp <|> _comm

 
_char :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_char    = do{p <- getPosition; try(do{string "char"     ; notFollowedBy alphaNumBar}); return (p,CHAR)}

_delete :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_delete  = do{p <- getPosition; try(do{string "delete"  ; notFollowedBy alphaNumBar}); return (p,DELETE)} 

_infixl :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_infixl  = do{p <- getPosition; try(do{string "infixl"   ; notFollowedBy alphaNumBar}); return (p,INFIXL)} 

_infixr :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_infixr  = do{p <- getPosition;try(do{string "infixr"   ; notFollowedBy alphaNumBar});  return (p,INFIXR)} 

_void :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_void    = do{p <- getPosition; try(do{string "void"    ; notFollowedBy alphaNumBar}); return (p,VOID)} 

_cnstnt :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_cnstnt  = do{p <- getPosition; try(do{string "constant" ; notFollowedBy alphaNumBar}); return (p,CNSTNT)} 

_const :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_const   = do{p <- getPosition; try(do{string "const"    ; notFollowedBy alphaNumBar}); return (p,CONST)} 

_ident :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_ident   = do{p <- getPosition; x <- identifier;return(p,IDENT x)}

_num :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_num     = do{p <- getPosition; x <- uint';     return(p,NUM x)}

_comm :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_comm    = do{p <- getPosition; x <- blockComm;return(p,COMM x)}

_sp :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_sp      = do{p <- getPosition; x <- many1 space;return(p,SP x)}

_op :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_op      = do{p <- getPosition; x <- operator;return(p,OP x)}


showTok :: Tok -> String
showTok  CHAR     = "token "     ++show "char"
showTok  DELETE   = "token "     ++show "delete"
showTok (IDENT i) = "identifier "++show i
showTok (NUM n)   = "number "    ++show n
showTok  PAREN    = "token "     ++show "("
showTok  NERAP    = "token "     ++show ")"
showTok  BRACE    = "token "     ++show "{"
showTok  ECARB    = "token "     ++show "}"
showTok  SCOLON   = "token "     ++show ";"
showTok (COMM s)  = "comment "   ++show ("/*"++s++"*/")
showTok (OP s)    = "operator "  ++show s
showTok INFIXL    = "token "     ++show "infixl"
showTok INFIXR    = "token "     ++show "infixr"
showTok VOID      = "token "     ++show "void"
showTok CONST     = "token "     ++show "const"
showTok (SP _)    = "space "     
showTok CNSTNT    = "token "     ++show "constant"