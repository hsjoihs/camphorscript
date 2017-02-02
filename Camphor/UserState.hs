{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.UserState
(Fixity(..)
,UserState(..)
,emptyState,containsIdent,addIdent,removeIdent,addOpFixity,getOpName,containsOp,findOpContents,matches
)where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Base_Step2_2
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import qualified Data.Map as M


data Fixity = InfixL Integer Oper | InfixR Integer Oper deriving(Show,Eq) 
type V_and_F_list = M.Map Ident (Either () [(TypeList, Sent)])
type OpList = M.Map Oper OpInfo
type OpInfo = (Fixity,[(TypeList,TypeList, Sent)])
data UserState = UserState V_and_F_list OpList
{-

UserState should contain the following things...

1. variable and function list, i.e.  M.Map Ident (Either () [(TypeList, Sent)] )
2. operator list, i.e.  M.Map Oper (Fixity,[(TypeList,TypeList, Sent)])

-}

getOpName :: Fixity -> Oper
getOpName (InfixL _ op) = op
getOpName (InfixR _ op) = op

emptyState :: UserState
emptyState = UserState M.empty M.empty

containsIdent :: UserState -> Ident -> Bool
containsIdent (UserState vflist _) ident = ident `M.member` vflist 

addIdent :: UserState -> Ident -> (Either () [(TypeList, Sent)]) -> UserState
addIdent    (UserState vflist oplist) ident dat = UserState (M.insert ident dat vflist) oplist 

removeIdent :: UserState -> Ident -> UserState
removeIdent (UserState vflist oplist) ident     = UserState (M.delete ident vflist) oplist

addOpFixity :: UserState -> Fixity -> UserState
addOpFixity (UserState vflist oplist) fixity = 
 UserState vflist (M.insert (getOpName fixity) (fixity,[]) oplist)
 
containsOp :: UserState -> Oper -> Bool
containsOp (UserState _ oplist) oper = oper `M.member` oplist

findOpContents :: UserState -> Oper -> Maybe (Fixity,[(TypeList,TypeList, Sent)])
findOpContents (UserState _ oplist) oper = M.lookup oper oplist

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