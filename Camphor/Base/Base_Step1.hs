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
 res <- lift $ parse parser1' (file ++ "--step1") (str <+> "\n") 
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
parser1' = many line <* eof


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
  return $ case ys of
   Right ys' -> DEFINE xs ys'
   Left  ys' -> OTHER("#define "++xs++ys')

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
data CurrentState = CS
 {
  defMacro :: Table,
  ifDepth  :: Integer,
  lineNum  :: Int,
  ok       :: Bool, -- whether to read a line or skip it
  skipDepth :: Integer,
  path     :: FilePath
 } deriving(Show,Eq,Ord)
 
initial :: Table -> FilePath -> CurrentState 
initial t f = CS{defMacro = t, ifDepth = 0,lineNum = 1,ok = True, skipDepth = -1, path = f}

makeErr' :: Message -> String -> Int -> Int -> WriterT Warnings (Either ParseError) a
makeErr' a b c d = lift(makeErr a b c d)

convert1 :: FilePath -> Includers -> [Pre7] -> WriterT Warnings (Either ParseError) Txt
convert1 file includers@(_,_,t) xs = pack . snd <$> conv1 includers (initial t file)xs -- fixme: txt


map2 :: (Monad m,Functor m) => (s -> s) -> (s -> m s2) -> (r -> a -> StateT s m [b]) -> r -> s -> [a] -> m(s2,[b]) 
map2 _  f2 _ _ s []     = do{s2 <- f2 s;return(s2,[])}
map2 f3 f2 f r s (x:xs) = do
 (b,s') <- runStateT (f r x) s 
 (b++) <$$> map2 f3 f2 f r (f3 s') xs
 
type SCWWEP = StateT CurrentState (WriterT Warnings (Either ParseError))  

modifyDepth :: (Integer -> Integer) -> SCWWEP String 
modifyDepth f = do
 stat@CS{ifDepth = depth} <- get
 put stat{ifDepth = f depth} 
 return "\n"
 
putFalseAndSink :: SCWWEP String
putFalseAndSink = do -- sink
 putFalse;
 modifyDepth(+1);
 
putFalse :: SCWWEP String
putFalse = do -- sink
 stat@CS{ifDepth = depth} <- get
 put stat{skipDepth = depth, ok = False}
 return "\n"
 
putTrue :: SCWWEP String
putTrue = do 
 stat <- get
 put stat{skipDepth = -1, ok = True}
 return "\n"

putTrueAndFloat :: SCWWEP String 
putTrueAndFloat = do 
 putTrue
 modifyDepth(+(-1));

 
conv1 :: Includers -> CurrentState -> [Pre7] -> WriterT Warnings (Either ParseError) (Table,String)
conv1 = map2 (\s -> s{lineNum = lineNum s + 1})empt c1
 where
  empt (CS table 0     _ True _ _) = return table
  empt (CS _     depth n _    _ f) 
   | depth>0                                                      = makeErr'(  Expect "#endif")(f++"--step1'") n 1
   | otherwise                                                    = makeErr'(UnExpect "#endif")(f++"--step1'") n 1  


c1 :: Includers -> Pre7 -> StateT CurrentState (WriterT Warnings (Either ParseError)) String
c1 _ (ERR msg) = do 
 stat@CS{ok = o, path = f} <- get
 if not o 
  then return "\n" 
  else lift $ makeErr'(Message$"#error "++msg)(f++"step1'") (lineNum stat) 1
 
c1 _ (WARN msg) = do
 stat@CS{ok = o, path = f} <- get
 if not o 
  then return "\n" 
  else do
   let lev = case words msg of ("crucial"  :_) -> Crucial; ("important":_) -> Important; ("verbose"  :_) -> Verbose;  _ -> Helpful  
   lift $ tellOne $ warn ("#warning "++msg) lev (newPos (f++"step1'") (lineNum stat) 1)
   return "\n"

c1 _ (DEFINE ide t) = do
 stat@CS{defMacro = table,ok = o, path = f} <- get
 case o of
  False -> return "\n"
  True
   | isJust(M.lookup ide table) -> lift $ makeErr'(Message$"C macro "++show ide++" is already defined")(f++"step1'") (lineNum stat) 1
   | otherwise                  -> put stat{defMacro = _tabl} >> return "\n"
   where _tabl = M.insert ide t table   
   
c1 _ (UNDEF  ide) = do
 stat@CS{defMacro = table, ok = o, path = f} <- get
 case o of 
  False -> return "\n"; 
  True
   | _tabl == table             -> lift $ makeErr'(Message$"C macro "++show ide++" is not defined")(f++"step1'") (lineNum stat) 1
   | otherwise                  -> put stat{defMacro = _tabl} >> return "\n"
   where _tabl = M.delete ide table
  
c1 _ IF1 = modifyDepth (+1)  -- in either case
c1 _ IF0 = do
 CS{ok = o} <- get
 if o 
  then putFalseAndSink
  else modifyDepth (+1)
 -- convert1' i(stat{lineNum = lineNum stat + 1, ifDepth = depth + 1 , ok = False, skipDepth = depth})xs
 
c1 _ (IFDEF ide) = do
 CS{ok = o,defMacro = table} <- get
 if o && isNothing(M.lookup ide table)
  then putFalseAndSink
  else modifyDepth (+1)

c1 _ (IFNDEF ide) = do
 CS{ok = o,defMacro = table} <- get
 if o && isJust(M.lookup ide table)
  then putFalseAndSink
  else modifyDepth (+1)
 
c1 _ ENDIF = do
 stat@CS{ok = o,ifDepth = depth,path = f} <- get
 case (o,depth) of
  (True ,0) -> lift $ makeErr'(UnExpect "#endif")(f++"step1'") (lineNum stat) 1 
  (True ,_) -> modifyDepth (+(-1)) >> return "\n"
  (False,_)
   | depth - 1 == skipDepth stat -> putTrueAndFloat 
   | otherwise                   -> modifyDepth (+(-1))
   
c1 _ ELSE = do
 stat@CS{ok = o,ifDepth = depth,path = f} <- get
 case (o,depth) of
  (True ,0) -> lift $ makeErr'(UnExpect "#else")(f++"step1'") (lineNum stat) 1 
  (True ,_) -> putFalse <* modify(\s -> s{skipDepth = depth-1}) 
  (False,_)
   | depth - 1 == skipDepth stat -> putTrue 
   | otherwise                   -> modify(\s -> s{skipDepth = depth}) >> return "\n"
   
c1 _ (OTHER t) = do
 CS{ok = o,defMacro = table} <- get
 if o 
  then do 
   replaced <- lift $ lift $ replaceBy table t   
   return (replaced ++ "\n")
  else return "\n"


c1 i@(j,_,_) (INCLU  fil) = do 
 stat@CS{ok = o} <- get
 if o then inclus2 fil j (stat,i) else return "\n"
 
c1 i@(_,j,_) (INCLU2 fil) = do
 stat@CS{ok = o} <- get
 if o then inclus2 fil j (stat,i) else return "\n"
   

   
 --  (b++) <$$> map2 f3 f2 f r (f3 s') xs
{--------------------
 | convert1' begins |
 --------------------}

{------------------
 | convert1' ends |
 ------------------} 
inclus2 :: FilePath -> FileToTxt ->  (CurrentState, Includers) -> StateT CurrentState (WriterT Warnings (Either ParseError)) String    
inclus2 fil j (CS{defMacro = table, ifDepth = depth,lineNum = n,path = f},i) = case M.lookup fil j of 
 Nothing  -> lift $ makeErr'(Message$"library "++show fil++" is not found")(f++"step1'") n 1 
 Just (dirf,txt) -> do
  let inclfile = dirf
  sets   <- lift $ lift $ parse parser1 (inclfile ++ "--step1") (txt <+> "\n")
  (newtable,text)      <- lift $ conv1 i (initial table inclfile)sets
  put CS{defMacro = newtable,ifDepth = depth  ,lineNum = n+1,ok = True ,skipDepth = -1,path = f}
  return("/*# LINE start "++show inclfile++" #*/\n\n"++text++"\n\n/*# LINE end   "++show inclfile++" #*/\n")  
 

 
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
  tChar       = try(char '\'' <:> noneOf "'\\" <:> string "'" ) <|> try(char '\'' <:> char '\\' <:> oneOf "abfnrtv0?'\"\\" <:> string "'")
  tString     = try(char '"'  <:> many(noneOf "\"") <++> string "\"")
  tSpecial    = strP $ oneOf "#$().;{\\}[]"
  tComment    = try(do{string"/*";xs <- manyTill anyChar (try (string "*/"));return$"/*"++(xs>>=escStar)++"*/" })
  tComment2   = try(do{string"//";xs <- manyTill anyChar eof;return$"/*"++(xs>>=escStar)++"*/"})
  tSpace      = try(many1 space)