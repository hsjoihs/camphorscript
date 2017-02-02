{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.Global
((<++>),(<:>),(<$$>),(>=>),(</>)

,Ident,Txt,MemSize,Address

,alphaBar,alphaNumBar,identifier,identifier'
,nbsp,nbsps,nbnls,nbnl
,spaces',spaces1',space',newline'
,uint,byte,uint'
,blockComm,lineComm,operator

,makeErr,Message(..),readEith,readMay,escStar,strP



)where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Control.Monad((>=>))
import Text.Parsec hiding(token)
import Data.Char(isSpace,ord)
import Control.Applicative hiding ((<|>),many)
import System.FilePath((</>))
import Text.Parsec.Error(newErrorMessage,Message(..))
import Text.Parsec.Pos(newPos)


{- 
   ######################################
   ## Chapter 1. Many useful operators ##
   ######################################
-}



infixr 5 <++>
(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

infixl 4 <$$>
(<$$>) :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)
(<$$>) = fmap . fmap 

-- Control.Monad.>=>
-- System.FilePath.</>



-- ####################### End of Chapter 1 ######################



{- 
   ######################################
   ##     Chapter 2. Type Synonyms     ##
   ######################################
-}

type Ident=String
type Txt=String
type MemSize=Integer
type Address=Integer

-- ####################### End of Chapter 2 ######################



{-
   ######################################
   ##        Chapter 3. Parsers        ##
   ######################################
-}

-- -- Chapter 3-1  Parsers related to identifier

alphaBar :: Stream s m Char => ParsecT s u m Char
alphaBar = (letter <|> char '_') <?> "letter or \"_\""

alphaNumBar :: Stream s m Char => ParsecT s u m Char
alphaNumBar = (alphaNum <|> char '_') <?> "letter, digit or \"_\""

identifier :: Stream s m Char => ParsecT s u m Ident
identifier = try(alphaBar <:> many alphaNumBar) <?> "identifier"

identifier' :: Stream s m Char => ParsecT s u m Ident
identifier' = (alphaBar <:> many alphaNumBar) <?> "identifier"


-- -- Chapter 3-2 Parsers related to non-breaking spaces
nbsp :: Stream s m Char => ParsecT s u m Char
nbsp = satisfy (\x->isSpace x && x/='\n') <?> "non-breaking space"

nbsps :: Stream s m Char => ParsecT s u m ()
nbsps = skipMany nbsp

nbnls :: Stream s m Char => ParsecT s u m () -- non-breaking space or comment
nbnls=skipMany nbnl

nbnl :: Stream s m Char => ParsecT s u m Char
nbnl = ( nbsp <?> "non-breaking space" ) <|> ( blockComm >> return ' ' )


-- -- Chapter 3-3 Parsers related to spaces
spaces' :: Stream s m Char => ParsecT s u m ()
spaces' = skipMany space'

spaces1' :: Stream s m Char => ParsecT s u m ()
spaces1' = skipMany1 space'

space' :: Stream s m Char => ParsecT s u m Char
space' = space <|> ( blockComm >> return ' ' ) <|> ( lineComm  >> return ' ' )

newline' :: Stream s m Char => ParsecT s u m ()
newline' = ( newline >> return () ) <|> ( lineComm >> return () )


-- -- Chapter 3-4 Parsers parsing numbers
uint :: Stream s m Char => ParsecT s u m String
uint = many1 digit <?> "unsigned integer"


uint' :: Stream s m Char => ParsecT s u m Integer
uint' = uint1 <|> _uint2 
 where 
  uint1 = (do{xs <- many1 digit'; return(foldl(\a b -> 10*a+b)0 xs) } <?> "unsigned integer")
  digit' = (
   do{char '0';return 0} <|> do{char '1';return 1} <|> do{char '2';return 2} <|>
   do{char '3';return 3} <|> do{char '4';return 4} <|> do{char '5';return 5} <|>
   do{char '6';return 6} <|> do{char '7';return 7} <|> do{char '8';return 8} <|>
   do{char '9';return 9} ) <?> "digit"

byte :: Stream s m Char => ParsecT s u m String
byte = many1 digit <|> (show <$> _uint2)
  
_uint2 :: Stream s m Char => ParsecT s u m Integer
_uint2 = try(do{char '\'';
   y <- (noneOf "\\\n" <|> parseEsc);
   char '\'';
   return . fromIntegral . ord $ y
   } <?> "character literal")
 where 
  parseEsc = do{char '\\';x<-oneOf "'\\abfnrtv\"?0";return(unEscSeq x)}
  unEscSeq 'a' = '\a'
  unEscSeq 'b' = '\b'
  unEscSeq 'f' = '\f'
  unEscSeq 'n' = '\n'
  unEscSeq 'r' = '\r'
  unEscSeq 't' = '\t'
  unEscSeq 'v' = '\v'
  unEscSeq '0' = '\0'
  unEscSeq x   = x -- '\?"

-- -- Chapter 3-5 Parsers parsing symbols
lineComm :: Stream s m Char => ParsecT s u m String
lineComm = do{string "//";comm<-many(noneOf "\n");newline;return (comm>>=escStar)}

blockComm :: Stream s m Char => ParsecT s u m String
blockComm = try(do{string "/*"; comm<-manyTill anyChar(try(string "*/"));return(comm>>=escStar)}) <?> "block comment"

operator :: Stream s m Char => ParsecT s u m String
operator = try(oneOf "!%&*+,-:<=>?@^|~" <:> many(oneOf "!%&*+,-:<=>?@^|~" <|> space ))


-- ####################### End of Chapter 3 ######################



{-
   ######################################
   ##       Chapter 4. Functions       ##
   ######################################
-}

makeErr :: Message -> SourceName -> Line -> Column -> Either ParseError b
makeErr msg pos x y = Left$newErrorMessage msg (newPos pos x y) 

readEith :: Read a => e -> String -> Either e a
readEith err s = case [x | (x,t) <- reads s, ("","") <- lex t] of [x] -> Right x; _ -> Left err

readMay :: Read a => String -> Maybe a
readMay s      = case [x | (x,t) <- reads s, ("","") <- lex t] of [x] -> Just x ; _ -> Nothing 

escStar :: Char -> String
escStar '*' = "_star_"
escStar x   = [x]

strP :: Stream s m Char => ParsecT s u m Char -> ParsecT s u m String
strP = fmap (:[])
 
