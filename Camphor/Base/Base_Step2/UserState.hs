{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.Base.Base_Step2.UserState
(UserState(),emptyState
,OpInfo,VFInfo(..) -- types

-- accesses private members
,getTmp,setTmp,clearTmp
,addVar,addFunSyn
,containsIdent,containsAnyIdent{- ,containsOp -}
{- ,addIdent-},removeIdent
,getVFContents
,getOpContents,addOpContents,addOpFixity
,addVFBlock,getTopVFBlock{- ,deleteTopVFBlock-},deleteTopVFBlock_ 
)where
import Camphor.SafePrelude
import Camphor.Base.Base_Step2.Type
import Camphor.Base.Base_Step2.Auxilary2
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import qualified Data.Map as M
import Text.Parsec.Pos(newPos)
import Camphor.NonEmpty as NE
import Control.Monad.State hiding(fix)

type OpInfo = (Fixity,[OpInstance])
data VFInfo = Variable | FunSyn [VFInstance] [SyntaxInstance] deriving(Show,Eq)

-- private
type VFList = NonEmpty(M.Map Ident2 VFInfo)
type OpList = M.Map Oper OpInfo
data UserState = UserState VFList OpList (Maybe TmpStat) deriving(Show)


emptyState :: UserState
emptyState = UserState deffun defop Nothing
 where
  deffun :: VFList
  deffun = nE(M.fromList[
   (readI ,FunSyn [(return(CHAR_AND,bbbb),Just$Single(newPos "__DEFAULT__" 0 0) (Rd  bbbb) )] []),
   (writeI,FunSyn [(return(CHAR_AND,bbbb),Just$Single(newPos "__DEFAULT__" 0 0) (Wrt bbbb) )] [])
   ])
  defop :: OpList
  defop = M.fromList[defau "+="$R_Pleq (Var aaaa) (Var nnnn), defau "-="$R_Mneq (Var aaaa) (Var nnnn)]
  defau :: String -> SimpleSent2 -> (Oper,OpInfo)
  defau a s = (wrap a,(InfixR 5 (wrap a),[(return(CHAR_AND,aaaa), return(CNSTNT_CHAR,nnnn), Just$Single(newPos "__DEFAULT__" 0 0)s)]))

-- checks if the current scope has already defined the variable; thus it does not search deeply  
containsIdent :: UserState -> Ident2 -> Bool
containsIdent (UserState (vf:|_) _ _) ident = ident `M.member` vf 

-- checks if any scope has already defined the variable; thus it DOES search deeply
containsAnyIdent :: UserState -> Ident2 -> Bool
containsAnyIdent (UserState vfs _ _) ident = any (ident `M.member`) (toList' vfs)

addVFBlock :: UserState -> UserState
addVFBlock (UserState vflist oplist tmp) = UserState (M.empty `cons` vflist) oplist tmp

getTopVFBlock :: UserState -> M.Map Ident2 VFInfo
getTopVFBlock (UserState (vf:|_) _ _) = vf

getTmp :: UserState -> Maybe TmpStat
getTmp (UserState _ _ tmp) = tmp

setTmp :: TmpStat -> UserState -> UserState
setTmp tmp (UserState a b _) = UserState a b (Just tmp)

clearTmp :: UserState -> UserState
clearTmp (UserState a b _) = UserState a b Nothing

deleteTopVFBlock :: UserState -> e -> Either e UserState
deleteTopVFBlock (UserState (_:|[]) _ _) e = Left e
deleteTopVFBlock (UserState (_:|(vf2:vfs)) oplist tmp) _ = Right$(UserState (vf2:|vfs) oplist tmp)

deleteTopVFBlock_ :: e -> StateT UserState (Either e) ()
deleteTopVFBlock_ e = StateT $ \s -> do
 s' <- deleteTopVFBlock s e
 return((),s')

-- addIdent :: UserState -> Ident2 -> VFInfo -> UserState
-- addIdent (UserState (vf:|vfs) oplist tmp) ident dat = UserState ((M.insert ident dat vf):|vfs) oplist tmp

addVar :: UserState -> Ident2 -> UserState
addVar (UserState (vf:|vfs) oplist tmp) ident = UserState ((M.insert ident Variable vf):|vfs) oplist tmp

addFunSyn :: UserState -> Ident2 -> [VFInstance] -> [SyntaxInstance] -> UserState
addFunSyn (UserState (vf:|vfs) oplist tmp) ident dat1 dat2 = UserState ((M.insert ident (FunSyn dat1 dat2) vf):|vfs) oplist tmp

getVFContents :: UserState -> Ident2 -> Maybe VFInfo
getVFContents (UserState vflist _ _) ident = searchBy (M.lookup ident) vflist

removeIdent :: UserState -> Ident2 -> UserState
removeIdent (UserState vflist oplist tmp) ident = case vflist of 
 vf:|[]        -> UserState (nE $ M.delete ident vf) oplist tmp
 vf:|(vf2:vfs) -> case M.lookup ident vf of
  Nothing -> removeIdent (UserState (vf2:|vfs) oplist tmp) ident
  _       -> UserState (M.delete ident vf :| (vf2:vfs)) oplist tmp

addOpFixity :: UserState -> Fixity -> UserState
addOpFixity (UserState vflist oplist tmp) fixity = 
 UserState vflist (M.insert (getOpName fixity) (fixity,[]) oplist) tmp
 
addOpContents :: UserState -> Oper -> OpInstance -> (e,e,e) -> Either e UserState
addOpContents stat@(UserState vflist oplist tmp) op (typelist1,typelist2,sent) (notfound,doubledefine,doubleparam) = case getOpContents stat op of
 Nothing -> Left notfound
 Just(fix,list)
  | conflict $ map snd $ toList' typelist1 ++ toList' typelist2 -> Left doubleparam
  | otherwise -> do
   newlist <- newlist' doubledefine
   return $ UserState vflist (M.insert op (fix,newlist) oplist) tmp
   where 
    newlist' :: e -> Either e [OpInstance]
    newlist' e  
     | any id [ typelist1 `overlaps` tlist1 && typelist2 `overlaps` tlist2 | (tlist1,tlist2,_) <- list ] = Left e
     | otherwise = Right $ (typelist1,typelist2,sent):list

-- containsOp :: UserState -> Oper -> Bool
-- containsOp (UserState _ oplist _) oper = oper `M.member` oplist

getOpContents :: UserState -> Oper -> Maybe OpInfo
getOpContents (UserState _ oplist _) oper = M.lookup oper oplist