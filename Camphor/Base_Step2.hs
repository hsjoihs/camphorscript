{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
{- Functional macro expansion -}
module Camphor.Base_Step2
(step2
--,parser2'
,sent
) where

import Camphor.Partial
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Global.Synonyms
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

type Extra = ()
type Sent  = Upgrade (Maybe Extra,SimpleSent)
type Sents  = [Sent]

type TypeList1 = (Type, Ident, [(Oper, Type, Ident)])

data SimpleSent =
 Char Ident | Del Ident | Scolon | Infl Integer String | Infr Integer String | Sp String | Comm String | 
 Func1 Ident TypeList1 Sent | Func2 Oper TypeList1 TypeList1 Sent | Call1 Ident ValueList |
 Call2 Oper ValueList ValueList deriving(Show)




simple :: SimpleSent -> Sent
simple x = Single(Nothing,x)
 
 
sent :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity Sent
sent = def <|> del <|> scl <|> infl <|> infr <|> spac <|> block <|> comm <|> func_def <|> op_def <|> func_call <|> op_call1

def :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity Sent
def = do{_char;   __; i <-_ident; __;_scolon;return(simple$Char i)}
--        char           abcd            ;

del :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity Sent
del = do{_delete; __; i <-_ident; __;_scolon;return(simple$Del i)}
--        delete         abcd            ;

scl :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity Sent
scl = _scolon >> return (simple Scolon)
--      ;

infl :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity Sent
infl = do{_infixl; __; n <-_num; __; _paren; __; o <-_op; __; _nerap; __; _scolon; return(simple$Infl n o)}
--         infixl          15              (            &&              )           ;          

infr :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity Sent
infr = do{_infixr; __; n <-_num; __; _paren; __; o <-_op; __; _nerap; __; _scolon; return(simple$Infr n o)}
--         infixr          15              (            &&              )           ;          

spac :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity Sent
spac = do{s <- _sp;  return(simple$Sp s)} 

comm :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity Sent
comm = do{s <- _comm;return(simple$Comm s)} 

block :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity Sent
block = do{_brace; ss <- many sent;_ecarb; return(Block ss)}
--           {      a; b; c;       }

func_def :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity Sent
func_def = do{
 name <- try(do{_void; __; n <- _ident; return n});  __; -- void clear
 _paren;               __;             -- (
 list1 <- getTypeList; __;             -- char& a
 _nerap;               __;             -- )
 m <- block;                           -- {         }
 return(simple$Func1 name list1 m)
 }
-- void 識別子(型 識別子【演算子 型 識別子】){【文】}

op_def :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity Sent
op_def = do{
 op <- try(do{_void; __; _paren; __; n <- _op; __; _nerap; return n});  __; -- void (+=)
 _paren;               __; -- (
 list1 <- getTypeList; __; -- char& a, char& b
 _scolon;              __; -- ;
 list2 <- getTypeList; __; -- constant char N
 _nerap;               __; -- )
 m <- block;               -- {    }
 return(simple$Func2 op list1 list2 m)
 }
-- void(演算子)(型 識別子【演算子 型 識別子】;型 識別子【演算子 型 識別子】){【文】}
 
func_call :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity Sent
func_call = do{
 name <- try(do{ n <- _ident; __; _paren; return n}); __; -- clear (
 vs   <- getValueList;                                __; -- a , b
 _nerap;                                              __; -- )
 ----- Semicolon is not parsed in order to handle while(a){}. Things like that are done while the conversion.
 return(simple$Call1 name vs);
 }

op_call1 :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity Sent
op_call1 = do{
 op <- try(do{ _paren; o <- _op;return o}); __; -- (+=
 _nerap; __; _paren;                        __; -- ) (
 vs1 <- getValueList;                       __; -- a , b
 _scolon;                                   __; -- ;
 vs2 <- getValueList;                       __; -- 1
 _nerap;                                    __; -- )
 _scolon; return(simple$Call2 op vs1 vs2);
 ----- Semicolon *is* parsed because troubles that can occur in normal functions cannot occur with operators
 }


{-
(値【演算子 値】)演算子(値 【演算子 値】);
			【】の中の全ての演算子の結合が呼び出される演算子の結合より大きい場合、カッコは省略可能。
		
-}
data Type = CNSTNT_CHAR | CONST_CHAR | CHAR_AND deriving(Show,Eq)

typ :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity Type
typ = 
 (do{_cnstnt; __; _char; return CNSTNT_CHAR}<?>"constant char")<|>
 (do{_const ; __; _char; return CONST_CHAR }<?>"const char"   )<|>
 (do{_char;   __; _and ; return CHAR_AND   }<?>"char&"        )
 
getTypeList :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity TypeList1
getTypeList = do
 g <- typ;      __;   
 h <- _ident;   __;    
 i <- many (do{a <- _op; __; b <- typ; __; c <- _ident; return(a,b,c)}); 
 return(g,h,i)
 
data Value = Var Ident | Constant Integer deriving(Show,Eq)

value :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity Value
value = (Var <$> _ident <?> "variable") <|> (Constant <$> _num <?> "unsigned integer or character literal")

type ValueList = (Value,[(Oper,Value)])
getValueList :: Stream s Identity (SourcePos, Tok) => ParsecT s u Identity ValueList
getValueList = do
 g <- value;  __;
 h <- many(do{a <- _op; __; b <- value; return(a,b)})
 return(g,h)
 

 
