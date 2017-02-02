{-# LANGUAGE NoImplicitPrelude , FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
{- C macro expansion -}
module Camphor.Base.Base_Step1
(step1
-- ,parser1
-- ,Pre7(..)
-- ,line
-- ,convert1
-- ,convert1'
-- ,replaceBy
-- ,parser1'5
-- ,token
) where

import Camphor.SafePrelude
import Camphor.Show
import Camphor.Global.Parsers
import Camphor.Global.Utilities
import Camphor.Global.Synonyms
import Text.Parsec hiding(token)
import Camphor.Lib
import Camphor.Transformer
import Camphor.Warn
import Text.Parsec.Pos(newPos)

import qualified Data.Map as M
{- C macro -}
step1 :: Includers -> FilePath -> Txt -> WriterT Warnings (Either ParseError) Txt
step1 includers file str = do
 res <- lift $ parse parser1' (file ++ "--step1") (str ++ "\n") 
 convert1 file includers res

-- PARSING
data Pre7 = 
 IFDEF Ident | IFNDEF Ident | 
 UNDEF Ident | ENDIF | ELSE | ERR String | 
 IF0 | IF1 | OTHER String | WARN String | 
 DEFINE Ident String | INCLU String | INCLU2 String  deriving (Show)

parser1 :: Stream s m Char => ParsecT s u m [Pre7]
parser1 = many line

parser1' :: Stream s m Char => ParsecT s u m [Pre7]
parser1' = do
 sents <- many line
 eof
 return sents

line :: Stream s m Char => ParsecT s u m Pre7
line = ifdef <|> ifndef <|> endif <|> else_dir <|> if0 <|> if1 <|> warning <|> err <|> define <|> undef <|> include <|> other
 where
  arg = do{nbnls;x<-identifier;nbnls;newline';return x}
  ifdef    = do{try(do{hash;string "ifdef" ;nbnl});x <- arg;return(IFDEF  x) }
  ifndef   = do{try(do{hash;string "ifndef";nbnl});x <- arg;return(IFNDEF x) }
  undef    = do{try(do{hash;string "undef" ;nbnl});x <- arg;return(UNDEF x) }
  endif    = do{try(do{hash;string "endif" });nbnls;newline';return ENDIF }
  else_dir = do{try(do{hash;string "else" }) ;nbnls;newline';return ELSE }
  if0      = do{try(do{hash;string "if_0" }) ;nbnls;newline';return IF0 }
  if1      = do{try(do{hash;string "if_1" }) ;nbnls;newline';return IF1 }
  warning  = do{try(do{hash;string "warning";nbnl});x <- many(noneOf "\n");newline';return(WARN x)} 
  err      = do{try(do{hash;string "error";nbnl});x <- many(noneOf "\n");newline';return(ERR x)} 

hash :: Stream s m Char => ParsecT s u m ()  
hash = do{nbnls;char '#';nbnls;}  
  
other' :: Stream s m Char => ParsecT s u m String
other' = do
 xs <- many(noneOf "\n/")
 do{newline;return xs} <|> do{
  char '/';
  do{char '/';ys<-many(noneOf "\n");newline;                        return (xs++"/*"++(ys>>=escStar)++"*/")} <|> 
  do{char '*';ys<-manyTill anyChar(try(string "*/"));zs <- other';return(xs++"/*"++(ys>>=escStar)++"*/"++zs)} <|>
  do{ys <- other'; return (xs++"/"++ys)}
  }  
  
other :: Stream s m Char => ParsecT s u m Pre7
other = OTHER <$> other'
 
  
{-functional not yet-}
define :: Stream s m Char => ParsecT s u m Pre7
define = do
  try(do{hash;string "define";nbnl})
  nbnls
  xs <- identifier'
  ys <- do{newline';return $ Right ""} <|> do{nbnl;nbnls;m<-many(noneOf "\n");newline';return $ Right m} <|> do{ks<-many(noneOf "\n");newline';return $ Left ks}
  case ys of
   Right ys' -> return $ DEFINE xs ys'
   Left  ys' -> return $ OTHER("#define "++xs++ys')

include :: Stream s m Char => ParsecT s u m Pre7
include  = do
 try(do{hash;string "include"})
 nbnls
 do{ char '<'; fi <- getFileName; char '>'; nbnls; newline'; (return . INCLU  . process) fi } <|>
  do{char '"'; fi <- getFileName; char '"'; nbnls; newline'; (return . INCLU2 . process) fi } 
 where
  process filename 
   | '.' `elem` filename = filename
   | otherwise           = filename ++ "." ++ default_lib_extension
  getFileName = do{nbnls; fi <- many (noneOf "\\/:*?\"<>|"); nbnls; return fi;}
  
  
-- CONVERSION

type Table = M.Map Ident String
type CurrentState = (Table,Integer,Int,Bool,Integer){-defined macro, how deep 'if's are, line num, whether to read a line,depth of skipping  -}

makeErr' :: Message -> String -> Int -> Int -> WriterT Warnings (Either ParseError) a
makeErr' a b c d = lift(makeErr a b c d)

convert1 :: FilePath -> Includers -> [Pre7] -> WriterT Warnings (Either ParseError) String
convert1 file includers@(_,_,t) xs = snd <$> convert1' file includers ((t,0,1,True,-1) ,xs) 

{--------------------
 | convert1' begins |
 --------------------}

convert1' :: FilePath -> Includers -> (CurrentState,[Pre7]) -> WriterT Warnings (Either ParseError) (Table,String)
convert1' _ _((table,0    ,_,True ,_),[]           ) = return(table,"")
convert1' f _((_    ,depth,n,_    ,_),[]           )                  
 | depth>0                                           = makeErr'(  Expect "#endif")(f++"--step1'") n 1
 | otherwise                                         = makeErr'(UnExpect "#endif")(f++"--step1'") n 1
convert1' f i((table,depth,n,False,o),IFDEF  _  :xs) = ('\n':) <$$> convert1' f i((table,depth+1,n+1,False,o    ),xs) 
convert1' f i((table,depth,n,False,o),IF0       :xs) = ('\n':) <$$> convert1' f i((table,depth+1,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),IF1       :xs) = ('\n':) <$$> convert1' f i((table,depth+1,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),IFNDEF _  :xs) = ('\n':) <$$> convert1' f i((table,depth+1,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),UNDEF  _  :xs) = ('\n':) <$$> convert1' f i((table,depth  ,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),INCLU  _  :xs) = ('\n':) <$$> convert1' f i((table,depth  ,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),INCLU2 _  :xs) = ('\n':) <$$> convert1' f i((table,depth  ,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),DEFINE _ _:xs) = ('\n':) <$$> convert1' f i((table,depth  ,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),OTHER  _  :xs) = ('\n':) <$$> convert1' f i((table,depth  ,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),WARN   _  :xs) = ('\n':) <$$> convert1' f i((table,depth  ,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),ERR    _  :xs) = ('\n':) <$$> convert1' f i((table,depth  ,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),ENDIF     :xs)
 | depth - 1 == o  {-reached the block you entered-} = ('\n':) <$$> convert1' f i((table,depth-1,n+1,True ,-1   ),xs)
 | otherwise                                         = ('\n':) <$$> convert1' f i((table,depth-1,n+1,False,o    ),xs)
convert1' f i((table,depth,n,False,o),ELSE      :xs)
 | depth - 1 == o  {-reached the block you entered-} = ('\n':) <$$> convert1' f i((table,depth  ,n+1,True ,-1   ),xs)
 | otherwise                                         = ('\n':) <$$> convert1' f i((table,depth  ,n+1,False,depth),xs)
convert1' f i((table,depth,n,True ,_),IFDEF  ide:xs)
 | isJust(M.lookup ide table)                        = ('\n':) <$$> convert1' f i((table,depth+1,n+1,True ,-1   ),xs)
 | otherwise                                         = ('\n':) <$$> convert1' f i((table,depth+1,n+1,False,depth),xs)
convert1' f i((table,depth,n,True ,_),IFNDEF ide:xs)
 | isJust(M.lookup ide table)                        = ('\n':) <$$> convert1' f i((table,depth+1,n+1,False,depth),xs)
 | otherwise                                         = ('\n':) <$$> convert1' f i((table,depth+1,n+1,True ,-1   ),xs)
convert1' f i((table,depth,n,True ,_),IF0       :xs) = ('\n':) <$$> convert1' f i((table,depth+1,n+1,False,depth),xs)
convert1' f i((table,depth,n,True ,_),IF1       :xs) = ('\n':) <$$> convert1' f i((table,depth+1,n+1,True ,-1   ),xs)  
 
-- non-skipping
convert1' f i((table,depth,n,True ,_),UNDEF  ide:xs)
 | _tabl==table                                      = makeErr'(Message$"C macro "++show ide++" is not defined")(f++"step1'") n 1
 | otherwise                                         = ('\n':) <$$> convert1' f i((_tabl,depth  ,n+1,True ,-1   ),xs)
 where _tabl = M.delete ide table
convert1' f i((table,depth,n,True ,_),ENDIF     :xs) 
 | depth == 0                                        = makeErr'(UnExpect "#endif")(f++"step1'") n 1 
 | otherwise                                         = ('\n':) <$$> convert1' f i((table,depth-1,n+1,True ,-1   ),xs)
convert1' f i((table,depth,n,True ,_),ELSE      :xs) 
 | depth == 0                                        = makeErr'(UnExpect "#else")(f++"step1'") n 1 
 | otherwise                                         = ('\n':) <$$> convert1' f i((table,depth  ,n+1,False,depth-1),xs)
convert1' f i((table,depth,n,True ,_),DEFINE ide t:xs)
 | isJust(M.lookup ide table)                        = makeErr'(Message$"C macro "++show ide++" is already defined")(f++"step1'") n 1
 | otherwise                                         = ('\n':) <$$> convert1' f i((_tabl,depth  ,n+1,True ,-1   ),xs)
 where _tabl = M.insert ide t table 
convert1' f i((table,depth,n,True ,_),OTHER t   :xs) = do
 replaced            <- lift $ replaceBy table t
 (newtable,result)   <- convert1' f i((table,depth,n+1,True ,-1 ),xs)
 return (newtable,replaced ++ "\n" ++ result)
convert1' f i@(j,_,_)((table,depth,n,True ,_),INCLU  fil:xs) = inclus fil j (table,depth,n,f,i,xs) 
convert1' f i@(_,j,_)((table,depth,n,True ,_),INCLU2 fil:xs) = inclus fil j (table,depth,n,f,i,xs) 

convert1' f i((table,depth,n,True ,_),WARN msg  :xs) = do
 let lev = case words msg of ("crucial"  :_) -> Crucial; ("important":_) -> Important; ("verbose"  :_) -> Verbose;  _ -> Helpful  
 tellOne(warn ("#warning "++msg) lev (newPos (f++"step1'") n 1))
 ('\n':) <$$> convert1' f i((table,depth  ,n+1,True ,-1   ),xs)
 
convert1' f _((_    ,_    ,n,True ,_),ERR  msg  :_ ) = makeErr'(Message$"#error "++msg)(f++"step1'") n 1

{------------------
 | convert1' ends |
 ------------------} 
 
 
inclus :: FilePath -> FileToTxt ->  (Table, Integer, Line, FilePath, Includers, [Pre7]) -> WriterT Warnings (Either ParseError) (Table,String) 
inclus fil j (table,depth,n,f,i,xs) = case M.lookup fil j of 
 Nothing  -> lift $ makeErr(Message$"library "++show fil++" is not found")(f++"step1'") n 1
 Just (dirf,txt) -> do
  let inclfile = dirf
  sets   <- lift $ parse parser1 (inclfile ++ "--step1") (txt ++ "\n")
  -- sets :: [Pre7]
  (newtable,text)      <- convert1' inclfile i ((table,0,0,True,-1) ,sets)
  (newtable',result)   <- convert1' f i((newtable,depth  ,n+1,True ,-1 ),xs)
  return(newtable',"/*# LINE start "++show inclfile++" #*/\n\n"++text++"\n\n/*# LINE end   "++show inclfile++" #*/\n"++result) 
 
 
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
token = tIdentifier <|> tComment <|> tComment2 <|> tOperator <|> -- Comm first, Op second
 tChar <|> tString <|> tSpecial <|> tSpace <|> tNumeral 
 where
  tIdentifier = identifier
  tNumeral    = try(many1 digit)
  tOperator   = unOp <$> operator
  tChar       = try(char '\'' <:> noneOf "'" <:> string "'"   )
  tString     = try(char '"'  <:> many(noneOf "\"") <++> string "\"")
  tSpecial    = strP $ oneOf "#$().;{\\}[]"
  tComment    = try(do{string"/*";xs<-manyTill anyChar (try (string "*/"));return$"/*"++(xs>>=escStar)++"*/" })
  tComment2   = try(do{string"//";xs<-manyTill anyChar eof;return$"/*"++(xs>>=escStar)++"*/"})
  tSpace      = try(many1 space)