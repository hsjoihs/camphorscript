{-# LANGUAGE FlexibleContexts, NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base.Base_Step2.Type
(ident_parser
,Tok(..)
,Type(..),Value(..)
,NonEmptyValue,ReplTable,CollisionTable
,PragmaData,ParserState
,Upgrade(..),Extra,Sent,Sent2,Sents,TypeList,ValueList,SimpleSent(..),Fixity(..),isVar,TmpStat
,Ident2(),toIdent2,unId,SimpleSent2(..),toSimpleSent2,toSent2
,bbbb,aaaa,nnnn,readI,writeI,tmpIdent
)where
import Camphor.Global.Synonyms
import Camphor.SafePrelude 
import Camphor.Global.Parsers
import Camphor.Global.Utilities
import Text.Parsec hiding(token)
import Camphor.SepList
import Camphor.NonEmpty
import qualified Data.Map as M
import Data.Char 


ident_parser :: Stream s m Char =>  ParsecT s u m (SourcePos,Tok)
ident_parser   = do
 p <- getPosition
 x <- identifier
 return(p,IDENT(Ident2 x))

bbbb, aaaa, nnnn, readI, writeI :: Ident2
bbbb = Ident2 "bbbb"
aaaa = Ident2 "aaaa"
nnnn = Ident2 "NNNN"
readI = Ident2 "read"
writeI = Ident2 "write"

tmpIdent :: Ident2 -> Integer -> Ident2
tmpIdent ident n = Ident2(unId ident ++ "__TMP_" ++ showNum n)


newtype Ident2 = Ident2{ unId :: String} deriving(Show,Ord,Eq)


toIdent2 :: String -> Either String Ident2
toIdent2 "" = Left ""
toIdent2 i@(x:xs) = maybeToEither i $ do
 guard $ (isAlpha x || x == '_')
 guard $ null[ a | a <- xs, not (isAlphaNum a), a /= '_']
 guard $ i `notElem` ["char","delete","infixl","infixr","void","constant","const" ]
 return(Ident2 i)


-- Auxilary
type NonEmptyValue = (Value,NonEmpty (Oper,Value))
type ReplTable = M.Map Ident2 Value
type CollisionTable = M.Map Ident2 (Ident2,Bool) -- bool : is this variable defined by the `using' pragma?

-- Base_Step2_2
data Upgrade a b = Single a b | Block a [Upgrade a b] deriving(Show,Eq)
type Extra = SourcePos
type Sent  = Upgrade Extra SimpleSent
type Sents = [Sent]
type TypeList = SepList Oper (Type,Ident2)
type ValueList = SepList Oper Value
type Sent2 = Upgrade Extra SimpleSent2

toSent2 :: Sent -> Sent2
toSent2 (Single a b) = Single a (toSimpleSent2 b)
toSent2 (Block a xs) = Block a (map toSent2 xs)

data SimpleSent =
 Scolon | Char Ident2 |   Del Ident2   |   Sp String   | Comm String |
 Pragma PragmaData   | Infl Fix Oper | Infr Fix Oper | 
 Func1 Ident2 TypeList Sent | Func1Nul Ident2 TypeList |
 Func2 Oper TypeList TypeList Sent   | 
 Func2Nul Oper TypeList TypeList     |
 Call1WithBlock  Ident2  ValueList  SourcePos  Sents  |   
 Call1 Ident2 ValueList     | Call2 Oper ValueList ValueList  | 
 Call5 ValueList  | Call3 Oper ValueList ValueList   |  Call4 [(Value,Oper)] ValueList |
 Pleq Ident2 Integer | Mneq Ident2 Integer |   Rd Ident2    |  Wrt Ident2  
 deriving(Show,Eq)
 
data SimpleSent2 = 
 R_Scolon | R_Char Ident2 |   R_Del Ident2   |   R_Sp String   | R_Comm String |
 R_Pragma PragmaData   | R_Infl Fix Oper | R_Infr Fix Oper | 
 R_Func1 Ident2 TypeList Sent | R_Func1Nul Ident2 TypeList |
 R_Func2 Oper TypeList TypeList Sent   | 
 R_Func2Nul Oper TypeList TypeList     |
 R_Call1WithBlock  Ident2  ValueList  SourcePos  Sents  |   
 R_Call1 Ident2 ValueList     | R_Call2 Oper ValueList ValueList  | 
 R_Call5 ValueList  | R_Call3 Oper ValueList ValueList   |  R_Call4 [(Value,Oper)] ValueList |
 R_Pleq Value Value | R_Mneq Value Value |   R_Rd Value    |  R_Wrt Value  
 deriving(Show,Eq)

toSimpleSent2 :: SimpleSent -> SimpleSent2
toSimpleSent2 Scolon                   = R_Scolon
toSimpleSent2 (Char i)                 = R_Char i
toSimpleSent2 (Del i)                  = R_Del i
toSimpleSent2 (Sp str)                 = R_Sp str
toSimpleSent2 (Comm str)               = R_Comm str 
toSimpleSent2 (Pragma p)               = R_Pragma p 
toSimpleSent2 (Infl f o)               = R_Infl f o 
toSimpleSent2 (Infr f o)               = R_Infr f o 
toSimpleSent2 (Func1 i t s)            = R_Func1 i t s 
toSimpleSent2 (Func1Nul i t)           = R_Func1Nul i t 
toSimpleSent2 (Func2 o t1 t2 s)        = R_Func2 o t1 t2 s 
toSimpleSent2 (Func2Nul o t1 t2)       = R_Func2Nul o t1 t2
toSimpleSent2 (Call1WithBlock i v p s) = R_Call1WithBlock i v p s
toSimpleSent2 (Call1 i v)              = R_Call1 i v
toSimpleSent2 (Call2 o v1 v2)          = R_Call2 o v1 v2
toSimpleSent2 (Call5 v)                = R_Call5 v
toSimpleSent2 (Call3 o v1 v2)          = R_Call3 o v1 v2
toSimpleSent2 (Call4 vo v2)            = R_Call4 vo v2
toSimpleSent2 (Pleq v i)               = R_Pleq (Var v) (Constant i)
toSimpleSent2 (Mneq v i)               = R_Mneq (Var v) (Constant i)
toSimpleSent2 (Rd v)                   = R_Rd (Var v)
toSimpleSent2 (Wrt v)                  = R_Wrt (Var v)


data Type = CNSTNT_CHAR | CONST_CHAR | CHAR_AND deriving(Show,Eq)
data Value = Var Ident2 | Constant Integer deriving(Show,Eq,Ord)
type ParserState = [SourcePos]

-- UserState
data Fixity = InfixL Integer Oper | InfixR Integer Oper deriving(Show,Eq) 
type TmpStat = [Ident2]

isVar :: Value -> Bool
isVar (Var _) = True; isVar _ = False

{----------------------------------
 |        end of Type.hs          |
 ----------------------------------}

-- PCS_Parser
data Tok = 
 CHAR  | DELETE | IDENT Ident2  |   NUM Integer   |  
 PAREN | NERAP  | BRACE | ECARB | SCOLON | CNSTNT |
 COMM String    |    OP Oper    | INFIXL | INFIXR |
 VOID  | CONST  |   SP String   | PRAGMA PragmaData         deriving(Show,Eq)