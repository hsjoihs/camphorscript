{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  -fno-warn-missing-signatures -fno-warn-unused-imports #-}

module Camphor.Step1
(step1
,example1
,err1
,example1'
) where


import Camphor.Global
import Text.Parsec hiding(token)
import Text.Parsec.Error
import Text.Parsec.Pos
import Control.Applicative hiding ((<|>),many)
import Control.Monad(join)

import qualified Data.Map as M

{- C macro  -}
step1 str=join(convert1 <$> (parse parser1 "step1"$str++"\n"))
parser1=sepBy line newline

data Pre7=IFDEF|IFNDEF|UNDEF|ENDIF|DEFINE|OTHER deriving(Show)

type Ident=String

line=ifdef<|>ifndef<|>endif<|>define<|>undef<|>other
 where
  ifdef  = do{try(do{nbsps;char '#';nbsps;string "ifdef" ;nbsp});nbsps;x<-identifier;return(IFDEF ,x,"")}
  ifndef = do{try(do{nbsps;char '#';nbsps;string "ifndef";nbsp});nbsps;x<-identifier;return(IFNDEF,x,"")}
  undef  = do{try(do{nbsps;char '#';nbsps;string "undef" ;nbsp});nbsps;x<-identifier;return(UNDEF ,x,"")}
  endif  = do{try(do{nbsps;char '#';nbsps;string "endif" });nbsps;return(ENDIF,"","")}
  other  = do{xs<-many(noneOf "\n");return(OTHER,"",xs)}
  
{-functional not yet-}
define=
 do
  try(do{nbsps;char '#';nbsps;string "define";nbsp})
  nbsps
  xs<-identifier'
  ys<-option ""(do{nbsp;nbsps;m<-many(noneOf "\n");return m})
  return(DEFINE,xs,ys)
  
  
err1="#define 1"
example1=
 "#ifndef ABC\n"++
 "#define ABC\n"++
 "\n"++
 "#define windows 1\n"++
 "#def(+=~)(char& a;char& b)\n"++
 "  while(b){\n"++
 "   b-=1;a+=1;\n"++
 "   }\n"++
 "#end_def\n"++
 "#define_ del(char& x) clear(x); delete x\n"++
 "char a;\n"++
 "#define C 3\n"++
 "a+=C; \n"++
 "#undef C\n"++
 "char C;\n"++
 "\n"++
 "#ifdef windows\n"++
 "char b;\n"++
 "b+=7;\n"++
 "#endif\n"++
 "\n"++
 "#ifndef windows\n"++
 "char x;\n"++
 "x+=8;\n"++
 "#endif\n"++
 "\n"++
 "#endif\n"
example1' = example1++example1

isJust(Just _)=True
isJust Nothing=False

type Table=M.Map Ident String
type CurrentState=(Table,Integer,Int,Bool,Integer){-defined macro, how deep 'if's are, line num, whether to read a line,depth of skipping  -}


convert1::[(Pre7,Ident,String)]->Either ParseError String
convert1 xs=convert1' ((M.empty,0,0,True,(-1)) ,xs)

convert1'::(CurrentState,[(Pre7,Ident,String)])->Either ParseError String

convert1' ((_    ,0    ,_,True ,_),[]               ) = Right ""
convert1' ((_    ,depth,n,_    ,_),[]               )                  
 | depth>0                                            = Left $newErrorMessage (  Expect "#endif")(newPos "step1" n 1) 
 | otherwise                                          = Left $newErrorMessage (UnExpect "#endif")(newPos "step1" n 1) 
 
 
convert1' ((table,depth,n,True ,_),(IFDEF ,ide,_):xs)
 | isJust(M.lookup ide table)                         = ('\n':)<$>convert1'((table,depth+1,n+1,True ,(-1) ),xs)
 | otherwise                                          = ('\n':)<$>convert1'((table,depth+1,n+1,False,depth),xs)
convert1' ((table,depth,n,True ,_),(IFNDEF,ide,_):xs)
 | isJust(M.lookup ide table)                         = ('\n':)<$>convert1'((table,depth+1,n+1,False,depth),xs)
 | otherwise                                          = ('\n':)<$>convert1'((table,depth+1,n+1,True ,(-1) ),xs)
convert1' ((table,depth,n,False,o),(IFDEF ,_  ,_):xs) = ('\n':)<$>convert1'((table,depth+1,n+1,False,o    ),xs)
convert1' ((table,depth,n,False,o),(IFNDEF,_  ,_):xs) = ('\n':)<$>convert1'((table,depth+1,n+1,False,o    ),xs)


convert1' ((table,depth,n,True ,_),(UNDEF ,ide,_):xs)
 | _tabl==table                                       = Left $newErrorMessage (UnExpect$"C macro "++show ide)(newPos "step1" n 1) 
 | otherwise                                          = ('\n':)<$>convert1'((_tabl,depth  ,n+1,True ,(-1) ),xs)
 where _tabl = M.delete ide table
convert1' ((table,depth,n,False,o),(UNDEF ,_  ,_):xs) = ('\n':)<$>convert1'((table,depth  ,n+1,False,o    ),xs)

convert1' ((table,depth,n,True ,_),(ENDIF ,_  ,_):xs) = ('\n':)<$>convert1'((table,depth-1,n+1,True ,(-1) ),xs)
convert1' ((table,depth,n,False,o),(ENDIF ,_  ,_):xs)
 | depth-1==o                                         = ('\n':)<$>convert1'((table,depth-1,n+1,True ,(-1) ),xs)
 | otherwise                                          = ('\n':)<$>convert1'((table,depth-1,n+1,False,o    ),xs)
 
 
convert1' ((table,depth,n,True ,_),(DEFINE,ide,t):xs)
 | isJust(M.lookup ide table)                         = Left $newErrorMessage (Message$"C macro "++show ide++" is already defined")(newPos "step1" n 1) 
 | otherwise                                          = ('\n':)<$>convert1'((_tabl,depth  ,n+1,True ,(-1) ),xs)
 where _tabl = M.insert ide t table
convert1' ((table,depth,n,False,o),(DEFINE,_  ,_):xs) = ('\n':)<$>convert1'((table,depth  ,n+1,False,o    ),xs)

convert1' ((table,depth,n,False,o),(OTHER ,_  ,_):xs) = ('\n':)<$>convert1'((table,depth  ,n+1,False,o    ),xs)

convert1' ((table,depth,n,True ,_),(OTHER ,_  ,t):xs) = (\x->convert1'5 table t++"\n"++x)<$>convert1'((table,depth,n,True ,(-1) ),xs)    

{- macro conversion-}
convert1'5::Table->String->String
convert1'5 table str = (\(Right x)->x)(parse parser1'5 "" str)>>=repl
 where 
  repl::String->String
  repl x=maybe x id (M.lookup x table)

parser1'5=many token

token = tIdentifier<|>tOperator<|>tChar<|>tString<|>tSpecial<|>tSpace<|>tNumeral

tIdentifier = identifier
tNumeral    = try(many1 digit)
tOperator   = try(oneOf "!%&*+,-/:<=>?@^|~" <:> many(oneOf "!%&*+,-/:<=>?@^|~" <|> space ))
tChar       = try(char '\'' <:> noneOf "'" <:> string "'"   )
tString     = try(char '"'  <:> many(noneOf "\"")<++> string "\"")
tSpecial    = strP$oneOf "#$().;{\\}[]"

tSpace      = try(many1 space)


