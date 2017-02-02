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

import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Global
import Text.Parsec hiding(token)
import Text.Parsec.Error
import Text.Parsec.Pos
import Control.Applicative hiding ((<|>),many)

import qualified Data.Map as M

{- C macro  -}
step1 :: (M.Map FilePath Txt) -> FilePath -> Txt -> Either ParseError Txt
step1 includer file str = parse parser1' (file ++ "--step1") (str ++ "\n") >>= convert1 file includer

-- PARSING

data Pre7 = IFDEF | IFNDEF | UNDEF | ENDIF | ELSE | DEFINE | INCLU | OTHER deriving (Show)
type Set = (Pre7,Ident,String)

parser1 :: Stream s m Char => ParsecT s u m [Set]
parser1 = many line

parser1' :: Stream s m Char => ParsecT s u m [Set]
parser1' = do{sents<-many line;eof;return sents;}

line :: Stream s m Char => ParsecT s u m Set
line = ifdef <|> ifndef <|> endif <|> else_dir <|> define <|> undef <|> include <|> other
 where
  ifdef    = (do{ try(do{nbnls;char '#';nbnls;string "ifdef" ;nbnl});nbnls;x<-identifier;nbnls;newline';return(IFDEF ,x,"") })
  ifndef   = (do{ try(do{nbnls;char '#';nbnls;string "ifndef";nbnl});nbnls;x<-identifier;nbnls;newline';return(IFNDEF,x,"") })
  undef    = (do{ try(do{nbnls;char '#';nbnls;string "undef" ;nbnl});nbnls;x<-identifier;nbnls;newline';return(UNDEF ,x,"") })
  endif    = (do{ try(do{nbnls;char '#';nbnls;string "endif" });nbnls;newline';return(ENDIF,"","") })
  else_dir = (do{ try(do{nbnls;char '#';nbnls;string "else" }) ;nbnls;newline';return(ELSE,"","") })

other :: Stream s m Char => ParsecT s u m Set
other = do
 xs <- many(noneOf("\n/"))
 do{newline;return(OTHER,"",xs)} <|> do{
  char '/';
  do{char '/';ys<-many(noneOf("\n"));newline;                                    return(OTHER,"",xs++"/*"++(ys>>=esc)++"*/")} <|> 
  do{char '*';ys<-manyTill anyChar(try(string "*/"));(_,_,zs)<-other;return(OTHER,"",xs++"/*"++(ys>>=esc)++"*/"++zs)} <|>
  do{(_,_,ys) <- other; return(OTHER,"",xs++"/"++ys) }
  }
 

  
  
  
{-functional not yet-}
define :: Stream s m Char => ParsecT s u m Set
define = do
  try(do{nbnls;char '#';nbnls;string "define";nbnl})
  nbnls
  xs <- identifier'
  ys <- do{newline';return $ Right ""} <|> do{nbnl;nbnls;m<-many(noneOf "\n");newline';return $ Right m} <|> do{ks<-many(noneOf "\n");newline';return $ Left ks}
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
 
 
convert1' f i((table,depth,n,False,o),(IFDEF ,_  ,_):xs) = ('\n':) <$$> convert1' f i((table,depth+1,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),(IFNDEF,_  ,_):xs) = ('\n':) <$$> convert1' f i((table,depth+1,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),(UNDEF ,_  ,_):xs) = ('\n':) <$$> convert1' f i((table,depth  ,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),(INCLU ,_  ,_):xs) = ('\n':) <$$> convert1' f i((table,depth  ,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),(DEFINE,_  ,_):xs) = ('\n':) <$$> convert1' f i((table,depth  ,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),(OTHER ,_  ,_):xs) = ('\n':) <$$> convert1' f i((table,depth  ,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),(ENDIF ,_  ,_):xs)
 | depth - 1 == o  {-reached a block that you entered -} = ('\n':) <$$> convert1' f i((table,depth-1,n+1,True ,(-1) ),xs)
 | otherwise                                             = ('\n':) <$$> convert1' f i((table,depth-1,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),(ELSE ,_  ,_):xs)
 | depth - 1 == o  {-reached a block that you entered -} = ('\n':) <$$> convert1' f i((table,depth  ,n+1,True ,(-1) ),xs)
 | otherwise                                             = ('\n':) <$$> convert1' f i((table,depth  ,n+1,False,depth),xs)
 
-- non-skipping

convert1' f i((table,depth,n,True ,_),(IFDEF ,ide,_):xs)
 | isJust(M.lookup ide table)                            = ('\n':) <$$> convert1' f i((table,depth+1,n+1,True ,(-1) ),xs)
 | otherwise                                             = ('\n':) <$$> convert1' f i((table,depth+1,n+1,False,depth),xs)
convert1' f i((table,depth,n,True ,_),(IFNDEF,ide,_):xs)
 | isJust(M.lookup ide table)                            = ('\n':) <$$> convert1' f i((table,depth+1,n+1,False,depth),xs)
 | otherwise                                             = ('\n':) <$$> convert1' f i((table,depth+1,n+1,True ,(-1) ),xs)
convert1' f i((table,depth,n,True ,_),(UNDEF ,ide,_):xs)
 | _tabl==table                                          = Left $newErrorMessage (Message$"C macro "++show ide++" is not defined")(newPos (f++"step1'") n 1) 
 | otherwise                                             = ('\n':) <$$> convert1' f i((_tabl,depth  ,n+1,True ,(-1) ),xs)
 where _tabl = M.delete ide table
convert1' f i((table,depth,n,True ,_),(INCLU ,_,fil):xs) = case M.lookup fil i of 
 Nothing  ->                                               Left $newErrorMessage (Message$"library "++show fil++" is not found")(newPos (f++"step1'") n 1) 
 Just txt ->                                               do
  let inclfile = lib_dir </> fil
  sets   <- parse parser1 (inclfile ++ "--step1") (txt ++ "\n")
  -- sets :: [Set]
  (newtable,text)      <- convert1' inclfile i ((table,0,0,True,(-1)) ,sets)
  (newtable',result)   <- convert1' f i((newtable,depth  ,n+1,True ,(-1) ),xs)
  return(newtable',"/* start of "++show inclfile++" */\n\n"++text++"\n\n/*  end  of "++show inclfile++" */\n"++result)
 
convert1' f i((table,depth,n,True ,_),(ENDIF ,_  ,_):xs) 
 | depth == 0                                            = Left $newErrorMessage (UnExpect$"#endif")(newPos (f++"step1'") n 1)  
 | otherwise                                             = ('\n':) <$$> convert1' f i((table,depth-1,n+1,True ,(-1)   ),xs)
convert1' f i((table,depth,n,True ,_),(ELSE  ,_  ,_):xs) 
 | depth == 0                                            = Left $newErrorMessage (UnExpect$"#else")(newPos (f++"step1'") n 1) 
 | otherwise                                             = ('\n':) <$$> convert1' f i((table,depth  ,n+1,False,depth-1),xs)
convert1' f i((table,depth,n,True ,_),(DEFINE,ide,t):xs)
 | isJust(M.lookup ide table)                            = Left $newErrorMessage (Message$"C macro "++show ide++" is already defined")(newPos (f++"step1'") n 1) 
 | otherwise                                             = ('\n':) <$$> convert1' f i((_tabl,depth  ,n+1,True ,(-1) ),xs)
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
 Just t  -> replaceBy (M.delete x table) t -- recursion (macro does not expand itself, which ensures that conversion terminates)



  
parser1'5 :: Stream s m Char => ParsecT s u m [String]
parser1'5 = many token


token :: Stream s m Char => ParsecT s u m String
token = tIdentifier <|> tOperator <|> tChar <|> tString <|> tSpecial <|> tSpace <|> tNumeral <|> tComment <|> tComment2
 where
  tIdentifier = identifier
  tNumeral    = try(many1 digit)
  tOperator   = try(oneOf "!%&*+,-:<=>?@^|~" <:> many(oneOf "!%&*+,-:<=>?@^|~" <|> space ))
  tChar       = try(char '\'' <:> noneOf "'" <:> string "'"   )
  tString     = try(char '"'  <:> many(noneOf "\"") <++> string "\"")
  tSpecial    = strP $ oneOf "#$().;{\\}[]"
  tComment    = try(do{string"/*";xs<-manyTill anyChar (try (string "*/"));return$"/*"++(xs>>=esc)++"*/" })
  tComment2   = try(do{string"//";xs<-manyTill anyChar eof;return$"/*"++(xs>>=esc)++"*/"})
  tSpace      = try(many1 space)

esc :: Char -> String
esc '*' = "_star_"
esc x   = [x]