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
tok = _char <|> _delete <|> _ident <|> _num <|> _scolon <|>
 _paren <|> _nerap <|> _brace <|> _ecarb <|>
 _comm <|> _op <|> _infixl <|> _infixr <|>
 _void <|> _sp <|> _cnstnt <|> _const 

_paren :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_paren  = do{char '('; p <- getPosition; return (p,PAREN)}
_nerap :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_nerap  = do{char ')'; p <- getPosition; return (p,NERAP)}
_brace :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_brace  = do{char '{'; p <- getPosition; return (p,BRACE)}
_ecarb :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_ecarb  = do{char '}'; p <- getPosition; return (p,ECARB)}
_scolon :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_scolon = do{char ';'; p <- getPosition; return (p,SCOLON)}



__ :: Stream s m Char => ParsecT s u m () 
__ = skipMany _nl

_nl :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_nl = _sp <|> _comm

 
_char :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_char    = do{try(do{string "char"     ; notFollowedBy alphaNumBar}); p <- getPosition; return (p,CHAR)}

_delete :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_delete  = do{try(do{string "_delete"  ; notFollowedBy alphaNumBar}); p <- getPosition; return (p,DELETE)} 

_infixl :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_infixl  = do{try(do{string "infixl"   ; notFollowedBy alphaNumBar}); p <- getPosition; return (p,INFIXL)} 

_infixr :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_infixr  = do{try(do{string "infixr"   ; notFollowedBy alphaNumBar}); p <- getPosition; return (p,INFIXR)} 

_void :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_void    = do{try(do{string "_void"    ; notFollowedBy alphaNumBar}); p <- getPosition; return (p,VOID)} 

_cnstnt :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_cnstnt  = do{try(do{string "constant" ; notFollowedBy alphaNumBar}); p <- getPosition; return (p,CNSTNT)} 

_const :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_const   = do{try(do{string "const"    ; notFollowedBy alphaNumBar}); p <- getPosition; return (p,CONST)} 

_ident :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_ident   = do{x<-identifier;p<-getPosition;return(p,IDENT x)}

_num :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_num     = do{x<-uint';p<-getPosition;return(p,NUM x)}

_comm :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_comm    = do{x<-blockComm;p<-getPosition;return(p,COMM x)}

_sp :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_sp      = do{x<-many1 space;p<-getPosition;return(p,SP x)}

_op :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
_op      = do{x<-operator;p<-getPosition;return(p,OP x)}


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