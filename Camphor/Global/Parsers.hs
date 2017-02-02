{-# LANGUAGE FlexibleContexts, NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.Global.Parsers
(alphaBar,alphaNumBar,identifier,identifier'
,nbsp,nbsps,nbnls,nbnl
,spaces',spaces1',space',newline'
,uint,byte,uint'
,blockComm,lineComm,operator,pragmaComm
,strP
)where
import Camphor.SafePrelude 
import Camphor.Global.Synonyms
import Camphor.Global.Operators
import Text.Parsec hiding(token)
import Data.Char(isSpace,ord)
import Camphor.NonEmpty


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
nbnls = skipMany nbnl

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
byte = many1 digit <|> (showNum <$> _uint2)
  
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
lineComm = do
 string "//"
 comm <- many(noneOf "\n")
 newline
 return (comm>>=escStar)

blockComm :: Stream s m Char => ParsecT s u m String
blockComm = try(do{string "/*"; comm <- manyTill anyChar(try(string "*/"));return(comm>>=escStar)}) <?> "block comment"

pragmaComm :: Stream s m Char => ParsecT s u m (Between PragmaData String)
pragmaComm = try(
 do
  string "/*#"
  comm <- manyTill anyChar(try(string "*/"))
  let comm' = comm>>=escStar
  case comm' of 
   [] -> return $ West('#':comm')
   (x:xs)
    | last' (x:|xs) == '#' -> return $ (East . words . init') (x:|xs)
    | otherwise            -> return $  West('#':comm')
 )

operator :: Stream s m Char => ParsecT s u m Oper
operator = wrap <$> try(oneOf "!%&*+,-:<=>?@^/|~" <:> many(oneOf "!%&/*+,-:<=>?@^|~" <|> space ))

-- ####################### End of Chapter 3 ######################

escStar :: Char -> String
escStar '*' = "_star_"
escStar x   = [x]

strP :: Stream s m Char => ParsecT s u m Char -> ParsecT s u m String
strP = fmap (:[])