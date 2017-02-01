{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind   -fno-warn-unused-imports #-}
{- symbolization -}
module Camphor.Base_Step7
(step7

,Com7(..)
,parser7
,convert7
,convert7'
) where

import Camphor.Global
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)
import Data.List(genericTake)
import Data.Functor.Identity


step7::Stream s Identity Char=>s->Either ParseError String
step7 str=convert7 <$> (parse parser7 "step7" str)  




 
data Com7=INC|DEC|MOV|LOOP|POOL|IN|OUT|NUL deriving(Show)

parser7::Stream s m Char=>ParsecT s u m [(Com7,String)]
parser7 = many sentences
 where
  sentences = inc<|>dec<|>loop<|>pool<|>mov<|>output<|>input<|>nul<|>comm
  inc    = do{string "inc";spaces;num<-option "1" uint;spaces;char ';'; return (INC,num)}
  dec    = do{string "dec";spaces;num<-option "1" uint;spaces;char ';'; return (DEC,num)}
  mov    = do{string "mov";spaces;num<-uint;spaces;char ';'; return (MOV,num)}
  loop   = do{string "loop";spaces;char ';'; return (LOOP,"")}
  pool   = do{string "pool";spaces;char ';'; return (POOL,"")}
  input  = do{string "_input";spaces;char ';'; return (IN,"")} {- "_input" rather than "input" to avoid 'try' -}
  output = do{string "output";spaces;char ';'; return (OUT,"")}
  nul    = do{sp<-many1 space;return (NUL,sp)}
  comm   = do{string "/*";comment<-many(noneOf "*");string "*/";return(NUL,"/*"++(comment>>=escape)++"*/")}
  escape '+' ="_plus_" 
  escape '-' ="_minus_" 
  escape ',' ="_comma_" 
  escape '.' ="_dot_" 
  escape '[' ="{(" 
  escape ']' =")}" 
  escape '>' ="_gt_" 
  escape '<' ="_lt_" 
  escape x = [x]

convert7::[(Com7,String)]->String
convert7 x=convert7'(0,x)

  
convert7'::(Integer,[(Com7,String)])->String
convert7' (_,[]             )  = ""
convert7' (n,((INC ,num):xs))  = genericTake(read num::Integer)(repeat '+') ++ convert7'(n,xs)
convert7' (n,((DEC ,num):xs))  = genericTake(read num::Integer)(repeat '-') ++ convert7'(n,xs)
convert7' (n,((LOOP,_  ):xs))  = "["                                        ++ convert7'(n,xs)
convert7' (n,((POOL,_  ):xs))  = "]"                                        ++ convert7'(n,xs)
convert7' (n,((IN  ,_  ):xs))  = ","                                        ++ convert7'(n,xs)
convert7' (n,((OUT ,_  ):xs))  = "."                                        ++ convert7'(n,xs)
convert7' (n,((NUL ,sp ):xs))  = sp                                         ++ convert7'(n,xs)
convert7' (n,((MOV ,num):xs))  
 |                   n<=num'    = genericTake(num'-n)(repeat '>')           ++ convert7'(num',xs)
 |                   otherwise  = genericTake(n-num')(repeat '<')           ++ convert7'(num',xs)
 where num' = read num ::Integer

