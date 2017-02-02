{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{- symbolization -}
module Camphor.Base_Step7
(step7

,Com7(..)
,parser7
,convert7
,convert7'
) where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Global
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)
import Data.List(genericTake)
import Data.Functor.Identity


step7 :: Stream s Identity Char => FilePath -> s -> Either ParseError Txt
step7 file str=convert7 <$> (parse parser7 (file++"--step7") str)  




data ComNum = INC | DEC | MOV
data Com7 =  LOOP | POOL | IN | OUT | NUL deriving(Show)
type Chunk = Either (ComNum,Integer) (Com7,String)
parser7 :: Stream s m Char => ParsecT s u m [Chunk]
parser7 = do{sents<-many sentences;eof;return sents}
 where
  sentences = inc <|> dec <|> loop <|> pool <|> mov <|> output <|> input <|> nul <|> comm
  inc    = do{string "inc";spaces;num<-option 1 uint';spaces;char ';'; return$Left(INC,num)}
  dec    = do{string "dec";spaces;num<-option 1 uint';spaces;char ';'; return$Left(DEC,num)}
  mov    = do{string "mov";spaces;num<-uint';spaces;char ';'; return$Left(MOV,num)}
  loop   = do{string "loop";spaces;char ';'; return$Right(LOOP,"")}
  pool   = do{string "pool";spaces;char ';'; return$Right(POOL,"")}
  input  = do{string "_input";spaces;char ';'; return$Right(IN,"")} {- "_input" rather than "input" to avoid 'try' -}
  output = do{string "output";spaces;char ';'; return$Right(OUT,"")}
  nul    = do{sp<-many1 space;return$Right(NUL,sp)}
  comm   = do{string "/*";comment<-many(noneOf "*");string "*/";return$Right(NUL,"/*"++(comment>>=escape)++"*/")}
  escape '+' = "_plus_" 
  escape '-' = "_minus_" 
  escape ',' = "_comma_" 
  escape '.' = "_dot_" 
  escape '[' = "{(" 
  escape ']' = ")}" 
  escape '>' = "_gt_" 
  escape '<' = "_lt_" 
  escape  x  = [x]

convert7 :: [Chunk]->String
convert7 x = convert7'(0,x)

  
convert7' :: (Integer,[Chunk])->String
convert7' (_,[]             )  = ""
convert7' (n,(Left (INC ,num):xs))  = genericTake num(repeat '+') ++ convert7'(n,xs)
convert7' (n,(Left (DEC ,num):xs))  = genericTake num(repeat '-') ++ convert7'(n,xs)
convert7' (n,(Right(LOOP,_  ):xs))  = "["                                        ++ convert7'(n,xs)
convert7' (n,(Right(POOL,_  ):xs))  = "]"                                        ++ convert7'(n,xs)
convert7' (n,(Right(IN  ,_  ):xs))  = ","                                        ++ convert7'(n,xs)
convert7' (n,(Right(OUT ,_  ):xs))  = "."                                        ++ convert7'(n,xs)
convert7' (n,(Right(NUL ,sp ):xs))  = sp                                         ++ convert7'(n,xs)
convert7' (n,(Left (MOV ,num):xs))  
 |                       n<=num     = genericTake(num-n)(repeat '>')             ++ convert7'(num,xs)
 |                       otherwise  = genericTake(n-num)(repeat '<')             ++ convert7'(num,xs)


