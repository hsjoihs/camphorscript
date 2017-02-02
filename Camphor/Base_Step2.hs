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

type Sent  = Upgrade (Maybe (),SimpleSent)
type Sents  = [Sent ]

type TypeList1 = (Type, Ident, [(Oper, Type, Ident)])

data SimpleSent =
 Char Ident | Del Ident | Scolon | Infl Integer String | Infr Integer String | Sp String | Comm String | 
 Func1 Ident TypeList1 Sent | Func2 Oper TypeList1 TypeList1 Sent deriving(Show)




simple :: SimpleSent -> Sent
simple x = Single(Nothing,x)
 
 
sent :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent)
sent = def <|> del <|> scl <|> infl <|> infr <|> spac <|> block <|> comm <|> func_def1

def :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent)
def = do{_char;   __; (IDENT i)<-_ident; __;_scolon;return(simple$Char i)}
--        char           abcd            ;

del :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent)
del = do{_delete; __; (IDENT i)<-_ident; __;_scolon;return(simple$Del i)}
--        delete         abcd            ;

scl :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent)
scl = _scolon >> return (simple Scolon)
--      ;

infl :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent)
infl = do{_infixl; __; (NUM n)<-_num; __; _paren; __; (OP o)<-_op; __; _nerap; __; _scolon; return(simple$Infl n o)}
--         infixl          15              (            &&              )           ;          

infr :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent)
infr = do{_infixr; __; (NUM n)<-_num; __; _paren; __; (OP o)<-_op; __; _nerap; __; _scolon; return(simple$Infr n o)}
--         infixr          15              (            &&              )           ;          

spac :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent)
spac = do{(SP s)<-_sp;return(simple$Sp s)} 

comm :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent)
comm = do{(COMM s)<-_sp;return(simple$Comm s)} 

block :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent)
block = do{_brace; ss<- many sent;_ecarb; return(Block ss)}
--           {      a; b; c;       }

data Type = CNSTNT_CHAR | CONST_CHAR | CHAR_AND deriving(Show,Eq)

typ :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity Type
typ = 
 (do{_cnstnt; __; _char;return CNSTNT_CHAR}<?>"constant char")<|>
 (do{_const ; __; _char;return CONST_CHAR }<?>"const char"   )<|>
 (do{_char;   __; _and ;return CHAR_AND   }<?>"char&"        )
 
func_def1 :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent)
func_def1 = do{
 (IDENT name) <- try(do{_void; __;n<-_ident;return n});  __; -- void ident
 _paren; __;                          -- (
 j <- typ;                            -- char&
 (IDENT k) <-_ident;                  -- a
 l <- many set;                       -- , const char b , const char c
 m <- block;
 return(simple$Func1 name (j,k,l) m)
 }
-- void 識別子(型 識別子【演算子 型 識別子】){【文】}

func_def2 :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Sent)
func_def2 = do{
 (OP op) <- try(do{_void; __;_paren; __; n<-_op; __; _nerap;return n});  __; -- void (+=)
 _paren; __;                          -- (
 
 j <- typ;                            -- char&
 (IDENT k) <-_ident;                  -- a
 l <- many set;                       -- 
 
 _scolon;                             -- ;
 g <- typ;
 (IDENT h) <- _ident;
 i <- many set; 
 m <- block;
 return(simple$Func2 op (j,k,l) (g,h,i) m)
 }
-- void 識別子(型 識別子【演算子 型 識別子】){【文】}
 
set ::  Stream s Identity (SourcePos, Tok) => ParsecT s u Identity (Oper, Type, Ident) 
set = do{(OP i)<-_op; j <- typ; (IDENT k) <- _ident; return(i,j,k)} 

{-
(値【演算子 値】)演算子(値 【演算子 値】);
			【】の中の全ての演算子の結合が呼び出される演算子の結合より大きい場合、カッコは省略可能。
		(演算子)(値【演算子 値】;値【演算子 値】);
		
		

		void(演算子)(型 識別子【演算子 型 識別子】;型 識別子【演算子 型 識別子】){【文】}
-}