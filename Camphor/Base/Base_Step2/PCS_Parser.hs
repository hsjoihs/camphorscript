{-# LANGUAGE FlexibleContexts , NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.Base.Base_Step2.PCS_Parser
(parser2'
,showTok
)where
import Camphor.SafePrelude 
import Camphor.Global.Parsers
import Text.Parsec hiding(token)
import Camphor.Base.Base_Step2.Type
import Camphor.Global.Synonyms

parser2' :: Stream s m Char => ParsecT s u m [(SourcePos,Tok)]
parser2' = concat <$> many tok <* eof

tok :: Stream s m Char => ParsecT s u m [(SourcePos,Tok)]
tok = _char <|> _delete  <|> _numB <|> _scolon <|> _syntax <|> _BLOCK <|>
 _paren <|> _nerap <|> _brace <|> _ecarb <|>
 _pragma <|> _comm <|> _op <|> -- _pragma -> _comm -> _op (IMPORTANT)
 _infixl <|> _infixr <|>
 _void <|> _sp <|> _cnstnt <|> _const <|> ident_parser'

ident_parser :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
ident_parser   = do
 p <- getPosition
 x <- identifier
 case toIdent2 x of
  Right i -> return(p,IDENT i)
  Left _ -> (oneOf "" <?> "identifier") >> return(p,SP " ")
 
 
ident_parser' :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
ident_parser' = (:[]) <$> ident_parser 
 
_paren :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
_paren  = do{p <- getPosition; char '('; return [(p,PAREN)]}
_nerap :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
_nerap  = do{p <- getPosition; char ')'; return [(p,NERAP)]}
_brace :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
_brace  = do{p <- getPosition; char '{'; return [(p,BRACE)]}
_ecarb :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
_ecarb  = do{p <- getPosition; char '}'; return [(p,SCOLON),(p,ECARB)]} -- auto semicolon insertion before '}'
_scolon :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
_scolon = do{p <- getPosition; char ';'; return [(p,SCOLON)]}

__ :: Stream s m Char => ParsecT s u m () 
__ = skipMany _nl

_nl :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
_nl = _sp <|> _comm

 
_char :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
_char    = do{p <- getPosition; try(do{string "char"     ; notFollowedBy alphaNumBar}); return [(p,CHAR)]}

_delete :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
_delete  = do{p <- getPosition; try(do{string "delete"  ; notFollowedBy alphaNumBar}); return [(p,DELETE)]} 

_infixl :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
_infixl  = do{p <- getPosition; try(do{string "infixl"   ; notFollowedBy alphaNumBar}); return [(p,INFIXL)]} 

_infixr :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
_infixr  = do{p <- getPosition;try(do{string "infixr"   ; notFollowedBy alphaNumBar});  return [(p,INFIXR)]} 

_void :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
_void    = do{p <- getPosition; try(do{string "void"    ; notFollowedBy alphaNumBar}); return [(p,VOID)]} 

_syntax :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
_syntax  = do{p <- getPosition; try(do{string "syntax"  ; notFollowedBy alphaNumBar}); return [(p,SYNTAX)]} 

_cnstnt :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
_cnstnt  = do{p <- getPosition; try(do{string "constant" ; notFollowedBy alphaNumBar}); return [(p,CNSTNT)]} 

_const :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
_const   = do{p <- getPosition; try(do{string "const"    ; notFollowedBy alphaNumBar}); return [(p,CONST)]} 

_BLOCK :: Stream s m Char => ParsecT s u m [(SourcePos,Tok)]
_BLOCK   = do{p <- getPosition; try(do{string "block"    ; notFollowedBy alphaNumBar}); return [(p,BLOCK)]} 


_numB :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
_numB     = do{p <- getPosition; x <- try uint'; return[(p,NUM x)]}

_comm :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
_comm    = do{p <- getPosition; x <- blockComm;return[(p,COMM x)]}

_pragma :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
_pragma  = do{p <- getPosition; x <- pragmaComm; case x of West c -> return[(p,COMM c)]; East pr -> return[(p,PRAGMA pr)]}

_sp :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
_sp      = do{p <- getPosition; x <- many1 space;return[(p,SP x)]}

_op :: Stream s m Char =>  ParsecT s u m [(SourcePos,Tok)]
_op      = do{p <- getPosition; x <- operator2 <|> operator;return[(p,OP x)]}

operator2 :: Stream s m Char => ParsecT s u m Oper
operator2 = try $ do
 char '<';        spaces;
 char '{';        spaces;
 i <- identifier; spaces;
 char '}';        spaces;
 char '>';        spaces;
 return $ wrap $ "<{"++i++"}>" -- fixme
 


showTok :: Tok -> String
showTok  CHAR     = "token "     ++showStr "char"
showTok  DELETE   = "token "     ++showStr "delete"
showTok (IDENT i) = "identifier "++showIdent i
showTok (NUM n)   = "number "    ++showNum n
showTok  PAREN    = "token "     ++showStr "("
showTok  NERAP    = "token "     ++showStr ")"
showTok  BRACE    = "token "     ++showStr "{"
showTok  ECARB    = "token "     ++showStr "}"
showTok  SCOLON   = "token "     ++showStr ";"
showTok (COMM s)  = "comment "   ++showStr ("/*"++s++"*/")
showTok (PRAGMA s) = "pragma "   ++showStr ("/*#"++unwords s++"#*/")
showTok (OP s)    = "operator "  ++showStr(unOp s)
showTok INFIXL    = "token "     ++showStr "infixl"
showTok INFIXR    = "token "     ++showStr "infixr"
showTok VOID      = "token "     ++showStr "void"
showTok SYNTAX    = "token "     ++showStr "syntax"
showTok BLOCK     = "token "     ++showStr "block"
showTok CONST     = "token "     ++showStr "const"
showTok (SP _)    = "space "     
showTok CNSTNT    = "token "     ++showStr "constant"