{-# LANGUAGE FlexibleContexts, NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base.Base_Step2.Type
(ident_parser
,Tok(..)
,Type(..),Value(..)
,NonEmptyValue,ReplTable,CollisionTable
,PragmaData,ParserState
,Upgrade(..),Extra,Sent,Sents,TypeList,ValueList,SimpleSent(..),Fixity(..),isVar,TmpStat
,Ident2(),toIdent2,unId
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
ident_parser   = do{p <- getPosition; x <- identifier;return(p,IDENT(Ident2 x))}

bbbb, aaaa, nnnn, readI, writeI :: Ident2
bbbb = Ident2 "bbbb"
aaaa = Ident2 "aaaa"
nnnn = Ident2 "NNNN"
readI = Ident2 "read"
writeI = Ident2 "write"

tmpIdent :: Ident2 -> Integer -> Ident2
tmpIdent ident n = Ident2(unId ident ++ "__TMP_" ++ showNum n)


newtype Ident2 = Ident2{ unId :: String} deriving(Show,Ord,Eq)
assert :: Bool -> Maybe ()
assert True = Just ()
assert False = Nothing

toIdent2 :: String -> Either String Ident2
toIdent2 "" = Left ""
toIdent2 i@(x:xs) = maybeToEither i $ do
 assert $ isAlpha x || x == '_'
 assert $ null[ a | a <- xs, not (isAlphaNum a), a /= '_']
 assert $ i `notElem` ["char","delete","infixl","infixr","void","constant","const" ]
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
data SimpleSent =
 Scolon | Char Ident2 |   Del Ident2   |   Sp String   | Comm String |
 Pragma PragmaData   | Infl Fix Oper | Infr Fix Oper | 
 Func1 Ident2 TypeList Sent | Func1Nul Ident2 TypeList |
 Func2 Oper TypeList TypeList Sent   | 
 Func2Nul Oper TypeList TypeList     |
 Call1WithBlock  Ident2  ValueList  SourcePos  Sents  |   
 Call1 Ident2 ValueList     | Call2 Oper ValueList ValueList  | 
 Call5 ValueList  | Call3 Oper ValueList ValueList   |  Call4 [(Value,Oper)] ValueList |
 Pleq Value Value | Mneq Value Value |   Rd Value    |  Wrt Value  
 deriving(Show,Eq)
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