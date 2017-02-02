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
step1 :: (M.Map FilePath Txt) -> FilePath -> Txt -> Either ParseError Txt
step1 includer file str = join (convert1 file includer <$> parse parser1 (file ++ "--step1") (str ++ "\n"))

-- PARSING

data Pre7 = IFDEF | IFNDEF | UNDEF | ENDIF | DEFINE | INCLU {-| LINE -}| OTHER deriving (Show)
type Set = (Pre7,Ident,String)

parser1 :: Stream s m Char => ParsecT s u m [Set]
parser1 = many line

line :: Stream s m Char => ParsecT s u m Set
line = ifdef <|> ifndef <|> endif <|> define <|> undef <|> include {-<|> line_dir-} <|> other
 where
  ifdef    = (do{ try(do{nbnls;char '#';nbnls;string "ifdef" ;nbsp});nbnls;x<-identifier;nbnls;newline';return(IFDEF ,x,"") })
  ifndef   = (do{ try(do{nbnls;char '#';nbnls;string "ifndef";nbsp});nbnls;x<-identifier;nbnls;newline';return(IFNDEF,x,"") })
  undef    = (do{ try(do{nbnls;char '#';nbnls;string "undef" ;nbsp});nbnls;x<-identifier;nbnls;newline';return(UNDEF ,x,"") })
  endif    = (do{ try(do{nbnls;char '#';nbnls;string "endif" });nbnls;newline';return(ENDIF,"","") })
  other    = (do{ xs<-many(noneOf "\n");newline';return(OTHER,"",xs) })
  {-line_dir = (do{ try(do{nbnls;char '#';nbnls;string "line"  ;nbsp});nbnls;n<-uint;  
   file' <- option "" (do{nbsp; nbnls; char '"'; file <- many(noneOf "\""); char '"'; return file});
   return(LINE,n,file')})-}
  
  
{-functional not yet-}
define :: Stream s m Char => ParsecT s u m Set
define = do
  try(do{nbnls;char '#';nbnls;string "define";nbsp})
  nbnls
  xs <- identifier'
  ys <- do{newline';return $ Right ""} <|> do{nbsp;nbnls;m<-many(noneOf "\n");newline';return $ Right m} <|> do{ks<-many(noneOf "\n");newline';return $ Left ks}
  case ys of
   Right ys' -> return (DEFINE,xs,ys')
   Left  ys' -> return (OTHER ,"","#define "++xs++ys')

include :: Stream s m Char => ParsecT s u m Set
include  = do
 try(do{nbnls;char '#';nbnls;string "include"})
 nbnls
 char '<'
 nbnls
 fi <- many (noneOf "\\/:*?\"<>|")
 nbnls
 char '>'
 nbnls
 newline'
 return(INCLU,"",fi) 
  
  
 
lift :: (Monad m) => (a -> b) -> m (n,a)-> m (n,b)
lift f mna = do
 (n,a) <- mna
 return(n,f a)
  



-- CONVERSION

type Table=M.Map Ident String
type CurrentState=(Table,Integer,Int,Bool,Integer){-defined macro, how deep 'if's are, line num, whether to read a line,depth of skipping  -}


convert1 :: FilePath ->(M.Map FilePath Txt) -> [Set] -> Either ParseError String
convert1 file includer xs = snd <$> convert1' file includer ((M.empty,0,0,True,(-1)) ,xs) 


convert1' :: FilePath -> (M.Map FilePath Txt) -> (CurrentState,[Set]) -> Either ParseError (Table,String)

convert1' _ _((table,0    ,_,True ,_),[]               ) = Right (table,"")
convert1' f _((_    ,depth,n,_    ,_),[]               )                  
 | depth>0                                              = Left $newErrorMessage (  Expect "#endif")(newPos (f++"--step1'") n 1) 
 | otherwise                                            = Left $newErrorMessage (UnExpect "#endif")(newPos (f++"--step1'") n 1) 
 
 
convert1' f i((table,depth,n,False,o),(IFDEF ,_  ,_):xs) = ('\n':) `lift` convert1' f i((table,depth+1,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),(IFNDEF,_  ,_):xs) = ('\n':) `lift` convert1' f i((table,depth+1,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),(UNDEF ,_  ,_):xs) = ('\n':) `lift` convert1' f i((table,depth  ,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),(INCLU ,_  ,_):xs) = ('\n':) `lift` convert1' f i((table,depth  ,n+1,False,o    ),xs)
-- convert1' f ((table,depth,n,False,o),(LINE  ,_  ,_):xs) = ('\n':) `lift` convert1' f((table,depth  ,n+1,False,o    ),xs) 
convert1' f i((table,depth,n,False,o),(ENDIF ,_  ,_):xs)
 | depth - 1 == o                                        = ('\n':) `lift` convert1' f i((table,depth-1,n+1,True ,(-1) ),xs)
 | otherwise                                             = ('\n':) `lift` convert1' f i((table,depth-1,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),(DEFINE,_  ,_):xs) = ('\n':) `lift` convert1' f i((table,depth  ,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),(OTHER ,_  ,_):xs) = ('\n':) `lift` convert1' f i((table,depth  ,n+1,False,o    ),xs) 
 
{-convert1' _ ((table,depth,_,True ,_),(LINE  ,num,f):xs) = (lin++) `lift` convert1' f((table,depth  ,nm ,True ,(-1) ),xs)
 where lin = "#line " ++ num ++ " " ++ show f ++ "\n"; nm=read num::Int  -}
convert1' f i((table,depth,n,True ,_),(IFDEF ,ide,_):xs)
 | isJust(M.lookup ide table)                            = ('\n':) `lift` convert1' f i((table,depth+1,n+1,True ,(-1) ),xs)
 | otherwise                                             = ('\n':) `lift` convert1' f i((table,depth+1,n+1,False,depth),xs)
convert1' f i((table,depth,n,True ,_),(IFNDEF,ide,_):xs)
 | isJust(M.lookup ide table)                            = ('\n':) `lift` convert1' f i((table,depth+1,n+1,False,depth),xs)
 | otherwise                                             = ('\n':) `lift` convert1' f i((table,depth+1,n+1,True ,(-1) ),xs)
convert1' f i((table,depth,n,True ,_),(UNDEF ,ide,_):xs)
 | _tabl==table                                          = Left $newErrorMessage (UnExpect$"C macro "++show ide)(newPos (f++"step1'") n 1) 
 | otherwise                                             = ('\n':) `lift` convert1' f i((_tabl,depth  ,n+1,True ,(-1) ),xs)
 where _tabl = M.delete ide table
convert1' f i((table,depth,n,True ,_),(INCLU ,_,fil):xs) = case M.lookup fil i of 
 Nothing  ->                                               Left $newErrorMessage (UnExpect$"library "++show fil)(newPos (f++"step1'") n 1) 
 Just txt ->                                               do
  let inclfile = lib_dir </> fil
  sets   <- parse parser1 (inclfile ++ "--step1") (txt ++ "\n")
  -- sets :: [Set]
  (newtable,text)      <- convert1' inclfile i ((table,0,0,True,(-1)) ,sets)
  (newtable',result)   <- convert1' f i((newtable,depth  ,n+1,True ,(-1) ),xs)
  return(newtable',"/* start of "++show inclfile++" */\n\n"++text++"\n\n/*  end  of "++show inclfile++" */\n"++result)
 
convert1' f i((table,depth,n,True ,_),(ENDIF ,_  ,_):xs) = ('\n':) `lift` convert1' f i((table,depth-1,n+1,True ,(-1) ),xs)
convert1' f i((table,depth,n,True ,_),(DEFINE,ide,t):xs)
 | isJust(M.lookup ide table)                            = Left $newErrorMessage (Message$"C macro "++show ide++" is already defined")(newPos (f++"step1'") n 1) 
 | otherwise                                             = ('\n':) `lift` convert1' f i((_tabl,depth  ,n+1,True ,(-1) ),xs)
 where _tabl = M.insert ide t table
convert1' f i((table,depth,n,True ,_),(OTHER ,_  ,t):xs) = do
 replaced            <- replaceBy table t
 (newtable,result)   <- convert1' f i((table,depth,n+1,True ,(-1) ),xs)
 return (newtable,replaced++"\n"++result)

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



  
parser1'5 :: Stream s m Char => ParsecT s u m [String]
parser1'5 = many token


token :: Stream s m Char => ParsecT s u m String
token = tIdentifier <|> tOperator <|> tChar <|> tString <|> tSpecial <|> tSpace <|> tNumeral <|> tComment <|> tComment2
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