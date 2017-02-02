{-# LANGUAGE FlexibleContexts, NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base.Base_Step2.Base_Step2_2
(parser2_2
)where
import Camphor.SepList hiding(reverse)
import Camphor.Base.Base_Step2.Type
import Camphor.SafePrelude
import Text.Parsec 
import Text.Parsec.Pos(initialPos) 
import Camphor.Base.Base_Step2.PCS_Parser2
import Camphor.Oper
import Camphor.TailSepList

parser2_2 :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sents
parser2_2 = do{xs <- many sent; eof; return (concat xs);} 

sent :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
sent = def <|> syntax <|> del <|> scl <|> bLOCK <|>
 infl <|> infr <|> spac <|> block2 <|> comm <|> pragma <|>
 func_def <|> op_def <|> func_call <|> syntax_call <|>
 op_call1 <|> op_call2 <|> op_call3 <|> op_call4 <|> op_call5 

 
doWith :: SourcePos -> [Ident2] -> Integer -> [Sent] 
doWith p is 0  =        [  Single p $ Char i | i <- is ] 
doWith p is nm = concat [ [Single p $ Char i,Single p $ Call2 (wrap "+=") (return$Var i) (return$Constant nm)] | i <- is ] 

syntax_ :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity (SourcePos,Ident2,Sent,Between TailTypeList TypeList)
syntax_ = do{
 p <- getPosition;
 _syntax;          __;  -- syntax
 name <- _ident;   __;  -- if
 _paren;           __;  -- (
 list <- eitherTL; __;  -- ~a
 _nerap;           __;  -- )
 _brace;           __;  -- {
 _BLOCK;           __;  -- block
 many ((_scolon >> return "") <|> _nl);
 _ecarb;           __;  -- }
 bl <- block;      __;  -- { ~~~~ }
 return (p,name,bl,list);
 }  
  
syntax :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent] 
syntax = try $ do
 (p,name,blk,list) <- syntax_
 case list of 
  West tl  -> return [ Single p $ Syntax1 name tl  blk ]
  East ttl -> return [ Single p $ Syntax2 name ttl blk ]

bLOCK :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
bLOCK = do{
 p <- getPosition;
 _BLOCK; __; 
 _scolon;
 return [Single p $ SynBlock]
 }
  
def :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
def = do{
 p <- getPosition;
 _char;            __;  -- char
 k <- singleDef;   __;  -- `singleDef`
 ks <- many(do{
  _comma;          __;  -- ,
  t <- singleDef;  __;  -- `singleDef`
  return t;
  });              __;
 _scolon;               -- ;
 return $ concat[ doWith p is nm | (is,nm) <- (k:ks) ]
 }

singleDef :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity ([Ident2],Integer)
singleDef = do{
 i <- _ident;                   __; -- abc
 (is,nm) <- option ([],0) (do{
  _eq;                          __; -- =
  js <- many(do{
   j <- _ident;                 __; -- def
   _eq;                         __; -- =
   return j;
   });
  n <- _num;                        -- 5
  return(js,n)
  });
 return(i:is,nm)
 }


del :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
del = do{p <- getPosition; _delete; __; i <-_ident; __;_scolon;return[Single p$Del i]}
--                          delete         abcd            ;

scl :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
scl = do{p <- getPosition; _scolon; return [Single p Scolon]}
--                           ;

infl :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
infl = do{p <- getPosition; _infixl; __; n <-_num; __; _paren; __; o <-_op; __; _nerap; __; _scolon; return[Single p$Infl n o]}
--         infixl          15              (            &&              )           ;          

infr :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
infr = do{p <- getPosition; _infixr; __; n <-_num; __; _paren; __; o <-_op; __; _nerap; __; _scolon; return[Single p$Infr n o]}
--         infixr          15              (            &&              )           ;          

spac :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
spac = do{p <- getPosition; s <- _sp;  return[Single p$Sp s]} 

comm :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
comm = do{p <- getPosition; s <- _comm;return[Single p$Comm s]} 

pragma :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
pragma = do
 p <- getPosition
 s <- _pragma
 case s of 
  []                    -> return()
  ["LINE","start",file] -> do_start file p
  ["LINE","end"  ,file] -> do_end file
  (_     :_ )           -> return() 
 return[Single p$Pragma s]
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
block' = do{p <- getPosition; _brace; ss <- many sent;_ecarb; return(p,concat ss)}
--           {      a; b; c;       }

block :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Sent
block = (uncurry Block) <$> block'

block2 :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
block2 = ((:[]) . uncurry Block) <$> block'

blockOrNull :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity (Between Sent ())
blockOrNull = try(East <$> block) <|> (_eq >> __ >> _zero >> __ >> _scolon >> return (West ()));

func_def :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
func_def = do{
 p <- getPosition; 
 name <- try(do{_void; __; n <- _ident; return n});  __; -- void clear
 _paren;               __;             -- (
 list1 <- getTypeList; __;             -- char& a
 _nerap;               __;             -- )
 m <- blockOrNull;  -- {   } or = 0 ;
 case m of 
  East b  -> return[Single p$Func1 name list1 b]; 
  West () -> return[Single p$Func1Nul name list1];
 }
-- void 識別子(型 識別子【演算子 型 識別子】){【文】}

op_def :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
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
  East b  -> return[Single p$Func2 op list1 list2 b]
  West () -> return[Single p$Func2Nul op list1 list2]
 }
-- void(演算子)(型 識別子【演算子 型 識別子】;型 識別子【演算子 型 識別子】){【文】}
 
func_call :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
func_call = try(do{
 p <- getPosition; 
 name <- try(do{ n <- _ident; __; _paren; return n}); __; -- clear (
 vs   <- getValueList;                                __; -- a , b
 _nerap;                                              __; -- )
 _scolon;                                                 -- ;
 return[Single p$Call1 name vs]
 }) 
 
syntax_call ::  Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
syntax_call = try(do{ 
 p <- getPosition;
 name <- try(do{ n <- _ident; __; _paren; return n}); __; -- if (
 vs   <- eitherVL;                                    __; -- ~a
 _nerap;                                              __; -- )
 (p2,m) <- block';
 case vs of 
  West tl  -> return[Single p$SynCall1 name tl  p2 m]
  East ttl -> return[Single p$SynCall2 name ttl p2 m]
 })

 
op_call1 :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
op_call1 = do{
 p <- getPosition; 
 op <- try(do{ _paren; __; o <- _op;return o}); __; -- (+=
 _nerap; __; _paren;                            __; -- ) (
 vs1 <- getValueList;                           __; -- a , b
 _scolon;                                       __; -- ;
 vs2 <- getValueList;                           __; -- 1
 _nerap;                                        __; -- )
 _scolon; return[Single p$Call2 op vs1 vs2];
 ----- Semicolon *is* parsed because troubles that can occur with normal functions cannot occur with operators
 }

---  (val [op val])op(val [op val]);
op_call2 :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
op_call2 = try(do{
 p <- getPosition; 
 _paren;                             __;  
 vs1 <- getValueList;                __;
 _nerap;                             __;
 
 op <- _op;                          __;
 
 _paren;                             __;
 vs2 <- getValueList;                __;
 _nerap;                             __;
 
 _scolon; return[Single p$Call2 op vs1 vs2];  --- Same as op_call1 because it always means the same thing
 })
 
 
--- (val [op val])op val [op val] ; 
op_call3 :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
op_call3 = try(do{
 p <- getPosition; 
 _paren;                             __;  
 vs1 <- getValueList;                __;
 _nerap;                             __;

 op <- _op;                          __;
 
 vs2 <- getValueList;                __;

 _scolon; return[Single p$Call3 op vs1 vs2]; 
 })

---   値【演算子 値】 演算子(値 【演算子 値】); or (値 【演算子 値】);
op_call4 :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
op_call4 = try(do{
 p <- getPosition; 
 xs <- many(do{a <- value; __; b <- _op; return(a,b)}); __;
 _paren;                                               __;
 vs <- getValueList;                                   __;
 _nerap;                                               __;
 _scolon; return[Single p$Call4 xs vs];
 
 })
 
---   値【演算子 値】 演算子 値 【演算子 値】 ; or 値;
op_call5 :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity [Sent]
op_call5 = try(do{
 p <- getPosition; 
 vs <- getValueList; __; 
 _scolon; 
 return[Single p$Call5 vs]
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
 i <- many (do{ a <- _op; __; b <- typ; __; c <- _ident; __; return(a,(b,c))}); 
 return$SepList (g,h) i
 
getTailTypeList :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity TailTypeList
getTailTypeList = TSL <$> many (do{ a <- _op; __; b <- typ; __; c <- _ident; __; return(a,(b,c))}); 

eitherTL :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity (Between TailTypeList TypeList)
eitherTL = West <$> try(getTypeList) <|> East <$> try(getTailTypeList) 

value :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity Value
value = (Var <$> _ident <?> "variable") <|> (Constant <$> _num <?> "unsigned integer or character literal")

getValueList :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity ValueList
getValueList = do
 g <- value;  __;
 h <- many(do{a <- _op; __; b <- value; __; return(a,b)})
 return$SepList g h
  
getTailValueList :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity TailValueList
getTailValueList = TSL <$> many(do{a <- _op; __; b <- value; __; return(a,b)});

eitherVL :: Stream s Identity (SourcePos, Tok) => ParsecT s ParserState Identity (Between TailValueList ValueList)
eitherVL = West <$> try(getValueList) <|> East <$> try(getTailValueList)  