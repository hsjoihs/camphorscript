{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base_Step2.Type
(NonEmptyValue,ReplTable,Ident
,Upgrade(..),Sent,Sents,Type(..),Value(..),TypeList,ValueList,Extra,SimpleSent(..)
,Tok(..)
)where
import Camphor.SepList
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
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
 VOID  | CONST  |   SP String                       deriving(Show,Eq)
 
-- Base_Step2_2
data Upgrade a = Single a | Block [Upgrade a] deriving(Show,Eq)
type Extra = SourcePos
type Sent  = Upgrade (Extra,SimpleSent)
type Sents = [Sent]
type TypeList = SepList Oper (Type,Ident)
type ValueList = SepList Oper Value
data SimpleSent =
 Char Ident | Del Ident | Scolon | Infl Fix Oper | Infr Fix Oper | Sp String | Comm String | 
 Func1 Ident TypeList Sent | Func2 Oper TypeList TypeList Sent | Call1 Ident ValueList | Call1WithBlock Ident ValueList Sents |
 Call2 Oper ValueList ValueList | Call3 Oper ValueList ValueList | Call4 [(Value,Oper)] ValueList | Call5 ValueList |
 Pleq Value Value | Mneq Value Value | Rd Value | Wrt Value
 deriving(Show,Eq)
data Type = CNSTNT_CHAR | CONST_CHAR | CHAR_AND deriving(Show,Eq)
data Value = Var Ident | Constant Integer deriving(Show,Eq)