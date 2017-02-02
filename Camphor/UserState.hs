{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.UserState
(Fixity(..)
,UserState(..)
,emptyState,containsIdent,addIdent,removeIdent,addOpFixity,getOpName,containsOp,findOpContents
)where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Base_Step2_2
import Camphor.Global.Synonyms
import qualified Data.Map as M


data Fixity = InfixL Integer Oper | InfixR Integer Oper deriving(Show,Eq) 
type V_and_F_list = M.Map Ident (Either () [(TypeList1, Sent)])
type OpList = M.Map Oper (Fixity,[(TypeList1,TypeList1, Sent)])
data UserState = UserState V_and_F_list OpList
{-

UserState should contain the following things...

1. variable and function list, i.e.  M.Map Ident (Either () [(TypeList1, Sent)] )
2. operator list, i.e.  M.Map Oper (Fixity,[(TypeList1,TypeList1, Sent)])

-}

getOpName :: Fixity -> Oper
getOpName (InfixL _ op) = op
getOpName (InfixR _ op) = op

emptyState :: UserState
emptyState = UserState M.empty M.empty

containsIdent :: UserState -> Ident -> Bool
containsIdent (UserState vflist _) ident = ident `M.member` vflist 

addIdent :: UserState -> Ident -> (Either () [(TypeList1, Sent)]) -> UserState
addIdent    (UserState vflist oplist) ident dat = UserState (M.insert ident dat vflist) oplist 

removeIdent :: UserState -> Ident -> UserState
removeIdent (UserState vflist oplist) ident     = UserState (M.delete ident vflist) oplist

addOpFixity :: UserState -> Fixity -> UserState
addOpFixity (UserState vflist oplist) fixity = 
 UserState vflist (M.insert (getOpName fixity) (fixity,[]) oplist)
 
containsOp :: UserState -> Oper -> Bool
containsOp (UserState _ oplist) oper = oper `M.member` oplist

findOpContents :: UserState -> Oper -> Maybe (Fixity,[(TypeList1,TypeList1, Sent)])
findOpContents (UserState _ oplist) oper = M.lookup oper oplist
