{-# LANGUAGE FlexibleContexts, NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base_Step2.Base_Step2_2
(parser2_2
)where
import Camphor.SepList hiding(reverse)
import Camphor.Base_Step2.Type
import Camphor.SafePrelude
import Text.Parsec 
import Text.Parsec.Pos(initialPos) 
import Camphor.Base_Step2.PCS_Parser2
import Data.Functor.Identity
import Control.Monad(when)

parser2_2 :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sents
parser2_2 = do{xs <- many sent; eof; return xs;} 

sent :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent
sent = def <|> del <|> scl <|>
 infl <|> infr <|> spac <|> block <|> comm <|> pragma <|>
 func_def <|> op_def <|> func_call <|> func_call_with_block <|>
 op_call1 <|> op_call2 <|> op_call3 <|> op_call4 <|> op_call5

def :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent
def = do{p <- getPosition; _char;   __; i <-_ident; __;_scolon;return(Single p$Char i)}
--        char           abcd            ;

del :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent
del = do{p <- getPosition; _delete; __; i <-_ident; __;_scolon;return(Single p$Del i)}
--        delete         abcd            ;

scl :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent
scl = do{p <- getPosition; _scolon; return (Single p Scolon)}
--      ;

infl :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent
infl = do{p <- getPosition; _infixl; __; n <-_num; __; _paren; __; o <-_op; __; _nerap; __; _scolon; return(Single p$Infl n o)}
--         infixl          15              (            &&              )           ;          

infr :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent
infr = do{p <- getPosition; _infixr; __; n <-_num; __; _paren; __; o <-_op; __; _nerap; __; _scolon; return(Single p$Infr n o)}
--         infixr          15              (            &&              )           ;          

spac :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent
spac = do{p <- getPosition; s <- _sp;  return(Single p$Sp s)} 

comm :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent
comm = do{p <- getPosition; s <- _comm;return(Single p$Comm s)} 

pragma :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent
pragma = do
 p <- getPosition
 s <- _pragma
 case s of 
  []                    -> return()
  ["LINE","start",file] -> do_start file p
  ["LINE","end"  ,file] -> do_end file
  (_     :_ )           -> return() 
 return(Single p$Pragma s)
 where 
  do_start file p = do
   stat <- getState
   putState(p:stat)
   setPosition $ initialPos file
   
  do_end file = do
   stat <- getState
   curpos <- getPosition
   when(sourceName curpos == file) $
    case stat of [] -> return(); (p:ps) -> do{putState ps; setPosition $ incSourceLine p 1 }
  

block' :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity (SourcePos,Sents)
block' = do{p <- getPosition; _brace; ss <- many sent;_ecarb; return(p,ss)}
--           {      a; b; c;       }

block :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent
block = (uncurry Block) <$> block'

blockOrNull :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity (Between Sent ())
blockOrNull = try(East <$> block) <|> (_eq >> __ >> _zero >> __ >> _scolon >> return (West ()));

func_def :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent
func_def = do{
 p <- getPosition; 
 name <- try(do{_void; __; n <- _ident; return n});  __; -- void clear
 _paren;               __;             -- (
 list1 <- getTypeList; __;             -- char& a
 _nerap;               __;             -- )
 m <- blockOrNull;  -- {   } or = 0 ;
 case m of 
  East b  -> return(Single p$Func1 name list1 b); 
  West () -> return(Single p$Func1Null name list1);
 }
-- void 識別子(型 識別子【演算子 型 識別子】){【文】}

op_def :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent
op_def = do{
 p <- getPosition; 
 op <- try(do{_void; __; _paren; __; n <- _op; __; _nerap; return n});  __; -- void (+=)
 _paren;               __; -- (
 list1 <- getTypeList; __; -- char& a, char& b
 _scolon;              __; -- ;
 list2 <- getTypeList; __; -- constant char N
 _nerap;               __; -- )
 m <- blockOrNull; -- {   } or = 0 ;
 case m of
  East b  -> return(Single p$Func2 op list1 list2 b)
  West () -> return(Single p$Func2Null op list1 list2)
 }
-- void(演算子)(型 識別子【演算子 型 識別子】;型 識別子【演算子 型 識別子】){【文】}
 
func_call_without_semicolon :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity (Ident,ValueList)
func_call_without_semicolon = do{
  
 name <- try(do{ n <- _ident; __; _paren; return n}); __; -- clear (
 vs   <- getValueList;                                __; -- a , b
 _nerap;                                              __; -- )
 ----- Semicolon is not parsed in order to handle while(a){}. Things like that are done while the conversion.
 return(name,vs);
 }
 
func_call :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent
func_call = try(do{
 p <- getPosition; 
 (name,vs) <- func_call_without_semicolon; __;
 _scolon; return(Single p$Call1 name vs)
 }) 

func_call_with_block ::  Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent 
func_call_with_block = try(do{ 
 p <- getPosition;
 (name,vs) <- func_call_without_semicolon; __;
 (p2,m) <- block';
 return(Single p$Call1WithBlock name vs p2 m)
 })

 
op_call1 :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent
op_call1 = do{
 p <- getPosition; 
 op <- try(do{ _paren; __; o <- _op;return o}); __; -- (+=
 _nerap; __; _paren;                            __; -- ) (
 vs1 <- getValueList;                           __; -- a , b
 _scolon;                                       __; -- ;
 vs2 <- getValueList;                           __; -- 1
 _nerap;                                        __; -- )
 _scolon; return(Single p$Call2 op vs1 vs2);
 ----- Semicolon *is* parsed because troubles that can occur in normal functions cannot occur with operators
 }

---  (val [op val])op(val [op val]);
op_call2 :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent
op_call2 = try(do{
 p <- getPosition; 
 _paren;                             __;  
 vs1 <- getValueList;                __;
 _nerap;                             __;
 
 op <- _op;                          __;
 
 _paren;                             __;
 vs2 <- getValueList;                __;
 _nerap;                             __;
 
 _scolon; return(Single p$Call2 op vs1 vs2);  --- Same as op_call1 because it always means the same thing
 })
 
 
--- (val [op val])op val [op val] ; 
op_call3 :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent
op_call3 = try(do{
 p <- getPosition; 
 _paren;                             __;  
 vs1 <- getValueList;                __;
 _nerap;                             __;

 op <- _op;                          __;
 
 vs2 <- getValueList;                __;

 _scolon; return(Single p$Call3 op vs1 vs2); 
 })

---   値【演算子 値】 演算子(値 【演算子 値】); or (値 【演算子 値】);
op_call4 :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent
op_call4 = try(do{
 p <- getPosition; 
 xs <- many(do{a <- value; __; b <- _op; return(a,b)}); __;
 _paren;                                               __;
 vs <- getValueList;                                   __;
 _nerap;                                               __;
 _scolon; return(Single p$Call4 xs vs);
 
 })
 
---   値【演算子 値】 演算子 値 【演算子 値】 ; or 値;
op_call5 :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent
op_call5 = try(do{
 p <- getPosition; 
 vs <- getValueList; __; _scolon; return(Single p$Call5 vs)
 })
typ :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Type
typ = 
 ( do{_cnstnt; __; _char; return CNSTNT_CHAR} <?> "constant char" )<|>
 ( do{_const ; __; _char; return CONST_CHAR } <?> "const char"    )<|>
 ( do{_char;   __; _and ; return CHAR_AND   } <?> "char&"         )
 
getTypeList :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity TypeList
getTypeList = do
 g <- typ;      __;   
 h <- _ident;   __;    
 i <- many (do{a <- _op; __; b <- typ; __; c <- _ident; return(a,(b,c))}); 
 return$SepList((g,h),i)

value :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Value
value = (Var <$> _ident <?> "variable") <|> (Constant <$> _num <?> "unsigned integer or character literal")

getValueList :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity ValueList
getValueList = do
 g <- value;  __;
 h <- many(do{a <- _op; __; b <- value; __; return(a,b)})
 return$SepList(g,h)