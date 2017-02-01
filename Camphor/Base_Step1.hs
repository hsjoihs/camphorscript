{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
{- C macro expansion -}
module Camphor.Base_Step1
(step1

,parser1
,Pre7(..)
,line
,convert1
,convert1'
,replaceBy
,parser1'5
,token
) where


import Camphor.Global
import Text.Parsec hiding(token)
import Text.Parsec.Error
import Text.Parsec.Pos
import Control.Applicative hiding ((<|>),many)
import Control.Monad(join)

import qualified Data.Map as M

{- C macro  -}
step1 :: String -> Either ParseError String
step1 str = join (convert1 <$> parse parser1 "step1" (str ++ "\n"))

-- PARSING

data Pre7 = IFDEF | IFNDEF | UNDEF | ENDIF | DEFINE | OTHER deriving (Show)
type Set = (Pre7,Ident,String)

parser1 :: Stream s m Char => ParsecT s u m [Set]
parser1 = many line

line :: Stream s m Char => ParsecT s u m Set
line = ifdef <|> ifndef <|> endif <|> define <|> undef <|> other
 where
  ifdef  = (do{ try(do{nbsps;char '#';nbsps;string "ifdef" ;nbsp});nbsps;x<-identifier;nbsps;newline;return(IFDEF ,x,"") })
  ifndef = (do{ try(do{nbsps;char '#';nbsps;string "ifndef";nbsp});nbsps;x<-identifier;nbsps;newline;return(IFNDEF,x,"") })
  undef  = (do{ try(do{nbsps;char '#';nbsps;string "undef" ;nbsp});nbsps;x<-identifier;nbsps;newline;return(UNDEF ,x,"") })
  endif  = (do{ try(do{nbsps;char '#';nbsps;string "endif" });nbsps;newline;return(ENDIF,"","") })
  other  = do{ xs<-many(noneOf "\n");newline;return(OTHER,"",xs) }
  
  
{-functional not yet-}
define :: Stream s m Char => ParsecT s u m Set
define =
 do
  try(do{nbsps;char '#';nbsps;string "define";nbsp})
  nbsps
  xs <- identifier'
  ys <- do{newline;return $ Right ""} <|> do{nbsp;nbsps;m<-many(noneOf "\n");newline;return $ Right m} <|> do{ks<-many(noneOf "\n");newline;return $ Left ks}
  case ys of
   Right ys' -> return (DEFINE,xs,ys')
   Left  ys' -> return (OTHER ,"","#define "++xs++ys')
  
  
 
  



-- CONVERSION

type Table=M.Map Ident String
type CurrentState=(Table,Integer,Int,Bool,Integer){-defined macro, how deep 'if's are, line num, whether to read a line,depth of skipping  -}


convert1 :: [Set] -> Either ParseError String
convert1 xs = convert1' ((M.empty,0,0,True,(-1)) ,xs)


convert1' :: (CurrentState,[Set]) -> Either ParseError String

convert1' ((_    ,0    ,_,True ,_),[]               ) = Right ""
convert1' ((_    ,depth,n,_    ,_),[]               )                  
 | depth>0                                            = Left $newErrorMessage (  Expect "#endif")(newPos "step1'" n 1) 
 | otherwise                                          = Left $newErrorMessage (UnExpect "#endif")(newPos "step1'" n 1) 
 
 
convert1' ((table,depth,n,False,o),(IFDEF ,_  ,_):xs) = ('\n':)<$>convert1'((table,depth+1,n+1,False,o    ),xs)
convert1' ((table,depth,n,False,o),(IFNDEF,_  ,_):xs) = ('\n':)<$>convert1'((table,depth+1,n+1,False,o    ),xs)
convert1' ((table,depth,n,False,o),(UNDEF ,_  ,_):xs) = ('\n':)<$>convert1'((table,depth  ,n+1,False,o    ),xs)
convert1' ((table,depth,n,False,o),(ENDIF ,_  ,_):xs)
 | depth - 1 == o                                     = ('\n':)<$>convert1'((table,depth-1,n+1,True ,(-1) ),xs)
 | otherwise                                          = ('\n':)<$>convert1'((table,depth-1,n+1,False,o    ),xs)
convert1' ((table,depth,n,False,o),(DEFINE,_  ,_):xs) = ('\n':)<$>convert1'((table,depth  ,n+1,False,o    ),xs)
convert1' ((table,depth,n,False,o),(OTHER ,_  ,_):xs) = ('\n':)<$>convert1'((table,depth  ,n+1,False,o    ),xs) 
 
 
convert1' ((table,depth,n,True ,_),(IFDEF ,ide,_):xs)
 | isJust(M.lookup ide table)                         = ('\n':)<$>convert1'((table,depth+1,n+1,True ,(-1) ),xs)
 | otherwise                                          = ('\n':)<$>convert1'((table,depth+1,n+1,False,depth),xs)
convert1' ((table,depth,n,True ,_),(IFNDEF,ide,_):xs)
 | isJust(M.lookup ide table)                         = ('\n':)<$>convert1'((table,depth+1,n+1,False,depth),xs)
 | otherwise                                          = ('\n':)<$>convert1'((table,depth+1,n+1,True ,(-1) ),xs)
convert1' ((table,depth,n,True ,_),(UNDEF ,ide,_):xs)
 | _tabl==table                                       = Left $newErrorMessage (UnExpect$"C macro "++show ide)(newPos "step1'" n 1) 
 | otherwise                                          = ('\n':)<$>convert1'((_tabl,depth  ,n+1,True ,(-1) ),xs)
 where _tabl = M.delete ide table
convert1' ((table,depth,n,True ,_),(ENDIF ,_  ,_):xs) = ('\n':)<$>convert1'((table,depth-1,n+1,True ,(-1) ),xs)
convert1' ((table,depth,n,True ,_),(DEFINE,ide,t):xs)
 | isJust(M.lookup ide table)                         = Left $newErrorMessage (Message$"C macro "++show ide++" is already defined")(newPos "step1'" n 1) 
 | otherwise                                          = ('\n':)<$>convert1'((_tabl,depth  ,n+1,True ,(-1) ),xs)
 where _tabl = M.insert ide t table
convert1' ((table,depth,n,True ,_),(OTHER ,_  ,t):xs) = do
 result   <- convert1'((table,depth,n+1,True ,(-1) ),xs)
 replaced <- replaceBy table t
 return (replaced++"\n"++result)

{- macro conversion-}
replaceBy :: Table -> String -> Either ParseError String
replaceBy table str = do
 xs       <- parsed 
 replaced <- mapM (replaceTokenBy table) xs
 return $ concat replaced
 where 
  parsed :: Either ParseError [String]
  parsed = parse parser1'5 "" str


    
replaceTokenBy :: Table -> String -> Either ParseError String -- maybe is harder to read than case of; thus rewrite
replaceTokenBy table x = case M.lookup x table of 
 Nothing -> Right x -- if not macro, no replacement
 Just t  -> replaceBy (M.delete x table) t -- recursion (macro does not expand itself, to ensure that it terminates)



  
parser1'5::Stream s m Char=>ParsecT s u m [String]
parser1'5=many token


token::Stream s m Char=>ParsecT s u m String
token = tIdentifier<|>tOperator<|>tChar<|>tString<|>tSpecial<|>tSpace<|>tNumeral<|>tComment<|>tComment2
 where
  tIdentifier = identifier
  tNumeral    = try(many1 digit)
  tOperator   = try(oneOf "!%&*+,-:<=>?@^|~" <:> many(oneOf "!%&*+,-:<=>?@^|~" <|> space ))
  tChar       = try(char '\'' <:> noneOf "'" <:> string "'"   )
  tString     = try(char '"'  <:> many(noneOf "\"")<++> string "\"")
  tSpecial    = strP$oneOf "#$().;{\\}[]"
  tComment    = try(do{string"/*";xs<-manyTill anyChar (try (string "*/"));return$"/*"++(xs>>=esc)++"*/" })
  tComment2   = try(do{string"//";xs<-manyTill anyChar eof;return$"/*"++(xs>>=esc)++"*/"})
  tSpace      = try(many1 space)

esc :: Char -> String
esc '*' = "_star_"
esc x   = [x]

