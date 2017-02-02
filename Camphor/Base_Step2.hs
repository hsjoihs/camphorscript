{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
{- Functional macro expansion -}
module Camphor.Base_Step2
(step2
--,parser2'

) where

import Camphor.Partial
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Global
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)
import Camphor.PCS_Parser

step2 :: a
step2 = undefined(parse sent "" "") 

data Upgrade a = Single a | Block [Upgrade a] deriving(Show)

type Sent b = Upgrade (Maybe b,SimpleSent)
type Sents b = [Sent b]

data SimpleSent =
 Char Ident | Del Ident | Scolon | Infl Integer String | Infr Integer String | Sp String deriving(Show)

simple :: SimpleSent -> Sent b
simple x = Single(Nothing,x)
 
 
sent :: Stream s m Char => ParsecT s u m (Sent b)
sent = def <|> del <|> scl <|> infl <|> infr <|> spac <|> block

def :: Stream s m Char => ParsecT s u m (Sent b)
def = do{_char;   __; i<-identifier; __;_scolon;return(simple$Char i)}
--        char           abcd            ;

del :: Stream s m Char => ParsecT s u m (Sent b)
del = do{_delete; __; i<-identifier; __;_scolon;return(simple$Del i)}
--        delete         abcd            ;

scl :: Stream s m Char => ParsecT s u m (Sent b)
scl = _scolon >> return (simple Scolon)
--      ;

infl :: Stream s m Char => ParsecT s u m (Sent b)
infl = do{_infixl; __; n<-uint'; __; _paren; __; o<-operator; __; _nerap; __; _scolon; return(simple$Infl n o)}
--         infixl      15              (            &&              )           ;          

infr :: Stream s m Char => ParsecT s u m (Sent b)
infr = do{_infixr; __; n<-uint'; __; _paren; __; o<-operator; __; _nerap; __; _scolon; return(simple$Infr n o)}
--         infixr      15              (            &&              )           ;          

spac :: Stream s m Char => ParsecT s u m (Sent b)
spac = do{s<-(blockComm <|> many1 space);return(simple$Sp s)} 

block :: Stream s m Char => ParsecT s u m (Sent b)
block = do{_brace; ss<- many sent;_ecarb; return(Block ss)}
--           {      a; b; c;       }

func1 :: ()
func1 = ()



{-

void abc( type (型 識別子【演算子 型 識別子】)    ) { 文    }


_ident <|> _num  <|> _paren    <|> _void  <|>  


_cnstnt <|> _const <|> _nerap <|> _ecarb <|> _op 
-}