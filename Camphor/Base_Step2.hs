{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
{- Functional macro expansion -}
module Camphor.Base_Step2
(step2
--,parser2'
,sent
) where

import Camphor.Partial
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Global
import Text.Parsec 
import Control.Applicative hiding ((<|>),many)
import Camphor.PCS_Parser2
import Data.Functor.Identity

step2 ::  FilePath -> Txt -> Either ParseError b
step2 file txt = do
 xs <- parse parser2'  (file ++ "-step2"  ) txt
 ys <- parse parser2_2 (file ++ "-step2-2") xs
 return ys

parser2_2 :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity b
parser2_2 = undefined 
 
data Upgrade a = Single a | Block [Upgrade a] deriving(Show)

type Sent b = Upgrade (Maybe b,SimpleSent)
type Sents b = [Sent b]

data SimpleSent =
 Char Ident | Del Ident | Scolon | Infl Integer String | Infr Integer String | Sp String  | Comm String deriving(Show)




simple :: SimpleSent -> Sent b
simple x = Single(Nothing,x)
 
 
sent :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent b)
sent = def <|> del <|> scl <|> infl <|> infr <|> spac <|> block <|> comm

def :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent b)
def = do{_char;   __; (IDENT i)<-_ident; __;_scolon;return(simple$Char i)}
--        char           abcd            ;

del :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent b)
del = do{_delete; __; (IDENT i)<-_ident; __;_scolon;return(simple$Del i)}
--        delete         abcd            ;

scl :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent b)
scl = _scolon >> return (simple Scolon)
--      ;

infl :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent b)
infl = do{_infixl; __; (NUM n)<-_num; __; _paren; __; (OP o)<-_op; __; _nerap; __; _scolon; return(simple$Infl n o)}
--         infixl      15              (            &&              )           ;          

infr :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent b)
infr = do{_infixr; __; (NUM n)<-_num; __; _paren; __; (OP o)<-_op; __; _nerap; __; _scolon; return(simple$Infr n o)}
--         infixr      15              (            &&              )           ;          

spac :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent b)
spac = do{(SP s)<-_sp;return(simple$Sp s)} 

comm :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent b)
comm = do{(COMM s)<-_sp;return(simple$Comm s)} 

block :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent b)
block = do{_brace; ss<- many sent;_ecarb; return(Block ss)}
--           {      a; b; c;       }
