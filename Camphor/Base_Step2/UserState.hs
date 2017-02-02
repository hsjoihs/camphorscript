{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base_Step2.UserState
(Fixity(..)
,UserState(..),VFInfo,OpInfo,VFList,OpList
,emptyState
,containsIdent,addIdent,removeIdent,getVFContents
,addOpFixity,getOpName,containsOp,getOpContents,matches,getFixValue
,isInfixL,isInfixR
,show',PrettyPrint
)where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Base_Step2.Base_Step2_2
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import qualified Data.Map as M

isInfixL :: Fixity -> Bool
isInfixL (InfixL _ _) = True
isInfixL (InfixR _ _) = False

isInfixR :: Fixity -> Bool
isInfixR = not . isInfixL

class PrettyPrint a where
 show' :: a -> String

instance PrettyPrint Fixity where
 show' (InfixL int op) = show op++"[infixl "++show int++"]"
 show' (InfixR int op) = show op++"[infixr "++show int++"]"
 
instance PrettyPrint Value where
 show'(Var x) = x
 show'(Constant n) = show n

data Fixity = InfixL Integer Oper | InfixR Integer Oper deriving(Show,Eq) 

type VFInfo = Either () [(TypeList, Sent)]
type OpInfo = (Fixity,[(TypeList,TypeList, Sent)])

type VFList = M.Map Ident VFInfo
type OpList = M.Map Oper OpInfo

data UserState = UserState VFList OpList

getFixValue :: Fixity -> Integer
getFixValue (InfixL fix _) = fix
getFixValue (InfixR fix _) = fix

getOpName :: Fixity -> Oper
getOpName (InfixL _ op) = op
getOpName (InfixR _ op) = op

emptyState :: UserState
emptyState = UserState M.empty M.empty

containsIdent :: UserState -> Ident -> Bool
containsIdent (UserState vflist _) ident = ident `M.member` vflist 

addIdent :: UserState -> Ident -> VFInfo -> UserState
addIdent      (UserState vflist oplist) ident dat = UserState (M.insert ident dat vflist) oplist 

getVFContents :: UserState -> Ident -> Maybe VFInfo
getVFContents (UserState vflist _) ident = M.lookup ident vflist

removeIdent :: UserState -> Ident -> UserState
removeIdent (UserState vflist oplist) ident     = UserState (M.delete ident vflist) oplist

addOpFixity :: UserState -> Fixity -> UserState
addOpFixity (UserState vflist oplist) fixity = 
 UserState vflist (M.insert (getOpName fixity) (fixity,[]) oplist)
 
containsOp :: UserState -> Oper -> Bool
containsOp (UserState _ oplist) oper = oper `M.member` oplist

getOpContents :: UserState -> Oper -> Maybe OpInfo
getOpContents (UserState _ oplist) oper = M.lookup oper oplist


matches :: ValueList -> TypeList -> Bool
matches (val,ovs) (typ,_,otis) 
 | length ovs /= length otis    = False -- wrong length
 | not(val `isTypeof` typ)      = False -- wrong type
 | all id $ zipMatch ovs otis   = True
 | otherwise                    = False
 where 
  zipMatch :: [(Oper,Value)] -> [(Oper,Type,a)] -> [Bool]
  zipMatch = zipWith (\(op2,val2)(op3,typ3,_) -> remSpace op2 == remSpace op3 && val2 `isTypeof` typ3)

isTypeof :: Value -> Type -> Bool
isTypeof _            CONST_CHAR  = True
isTypeof (Var      _) CHAR_AND    = True
isTypeof (Constant _) CNSTNT_CHAR = True
isTypeof _            _           = False