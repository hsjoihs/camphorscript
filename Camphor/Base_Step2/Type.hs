{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.Base_Step2.Type
(Type(..),Value(..),Ident
,NonEmptyValue,ReplTable
,Tok(..),PragmaData,ParserState
,Upgrade(..),Extra,Sent,Sents,TypeList,ValueList,SimpleSent(..),Fixity(..),isVar
)where
import Camphor.SepList
import Camphor.SafePrelude
import Camphor.Global.Synonyms
import Camphor.NonEmpty
import Text.Parsec 
import qualified Data.Map as M 

-- Auxilary
type NonEmptyValue = (Value,NonEmpty (Oper,Value))
type ReplTable = M.Map Ident Value

-- PCS_Parser
data Tok = 
 CHAR  | DELETE | IDENT Ident   |   NUM Integer   |  
 PAREN | NERAP  | BRACE | ECARB | SCOLON | CNSTNT |
 COMM String    |    OP Oper    | INFIXL | INFIXR |
 VOID  | CONST  |   SP String   | PRAGMA PragmaData         deriving(Show,Eq)

 
-- Base_Step2_2
data Upgrade a b = Single a b | Block a [Upgrade a b] deriving(Show,Eq)
type Extra = SourcePos
type Sent  = Upgrade Extra SimpleSent
type Sents = [Sent]
type TypeList = SepList Oper (Type,Ident)
type ValueList = SepList Oper Value
data SimpleSent =
 Char Ident | Del Ident | Scolon | Infl Fix Oper | Infr Fix Oper | Sp String | Comm String | Pragma PragmaData |
 Func1 Ident TypeList Sent | Func1Null Ident TypeList | Func2 Oper TypeList TypeList Sent | Func2Null Oper TypeList TypeList | 
 Call1 Ident ValueList | Call1WithBlock Ident ValueList SourcePos Sents |
 Call2 Oper ValueList ValueList | Call3 Oper ValueList ValueList | Call4 [(Value,Oper)] ValueList | Call5 ValueList |
 Pleq Value Value | Mneq Value Value | Rd Value | Wrt Value
 deriving(Show,Eq)
data Type = CNSTNT_CHAR | CONST_CHAR | CHAR_AND deriving(Show,Eq)
data Value = Var Ident | Constant Integer deriving(Show,Eq,Ord)
type ParserState = [SourcePos]

-- UserState
data Fixity = InfixL Integer Oper | InfixR Integer Oper deriving(Show,Eq) 

isVar :: Value -> Bool
isVar (Var _) = True; isVar _ = False