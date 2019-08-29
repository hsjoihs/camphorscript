{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.Base.Base_Step2.UserState
(UserState(),emptyState
,OpInfo,VFInfo(..),AddOpErr(..) -- types

-- accesses private members
,getTmp,setTmp,clearTmp
,addVar,addFunSyn
,containsIdent,containsAnyIdent{- ,containsOp -}
{- ,addIdent-},removeIfContains
,getVFContents
,getOpContents,addOpContents,addOpFixity
,addVFBlock,getTopVFBlock
)where
import Camphor.SafePrelude
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import Camphor.NonEmpty as NE
import Camphor.Base.Base_Step2.Type
import Camphor.Base.Base_Step2.Auxilary2

import qualified Data.Map as M
import Text.Parsec.Pos(newPos)



type OpInfo = (Fixity,[OpInstance])
data VFInfo = Variable | FunSyn [VFInstance] [SyntaxInstance] deriving(Show,Eq)
data AddOpErr = Notfound | Doubledefine | Doubleparam deriving(Show,Eq,Ord)

-- private
type VFBlock = M.Map Ident2 VFInfo
type VFList = NonEmpty VFBlock
type OpList = M.Map Oper OpInfo
data UserState = UserState {stVF :: VFList,  stOp :: OpList,  getTmp :: Maybe TmpStat} --deriving(Show)


-- empty state
emptyState :: UserState
emptyState = UserState{stVF = deffun, stOp = defop, getTmp = Nothing}
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

  
--- checks what a state contains 
-- checks if the current scope has already defined the variable; thus it does not search deeply  
containsIdent :: UserState -> Ident2 -> Bool
containsIdent UserState{stVF = (vf:|_)} ident = ident `M.member` vf 

-- checks if any scope has already defined the variable; thus it DOES search deeply
containsAnyIdent :: UserState -> Ident2 -> Bool
containsAnyIdent UserState{stVF = vfs} ident = any (ident `M.member`) (toList vfs)

-- containsOp :: UserState -> Oper -> Bool
-- containsOp (UserState _ oplist _) oper = oper `M.member` oplist


-- get what a state has
getTopVFBlock :: UserState -> M.Map Ident2 VFInfo
getTopVFBlock UserState{stVF = vf:|_} = vf

getVFContents :: UserState -> Ident2 -> Maybe VFInfo
getVFContents UserState{stVF = vflist} ident = searchBy (M.lookup ident) vflist

getOpContents :: UserState -> Oper -> Maybe OpInfo
getOpContents stat oper = M.lookup oper (stOp stat)



-- modifies stat
setTmp :: TmpStat -> UserState -> UserState
setTmp tmp stat = stat{getTmp = Just tmp}

clearTmp :: UserState -> UserState
clearTmp stat = stat{getTmp = Nothing}

addVFBlock :: UserState -> UserState
addVFBlock stat = stat{stVF = M.empty `cons` stVF stat}

modifyTop :: (VFBlock -> VFBlock) -> UserState -> UserState
modifyTop f stat@UserState{stVF = vf:|vfs}  = stat{stVF = f vf:|vfs}

addVar :: UserState -> Ident2 -> UserState
addVar stat ident = modifyTop (M.insert ident Variable) stat

addFunSyn :: UserState -> Ident2 -> [VFInstance] -> [SyntaxInstance] -> UserState
addFunSyn stat ident dat1 dat2 = modifyTop (M.insert ident (FunSyn dat1 dat2)) stat

removeIfContains :: UserState -> Ident2 -> Maybe UserState
removeIfContains stat ident 
 | stat `containsIdent` ident = Just$ modifyTop (M.delete ident) stat
 | otherwise = Nothing
 
addOpFixity :: UserState -> Fixity -> UserState
addOpFixity stat@UserState{stOp = oplist} fixity = stat{stOp = M.insert (getOpName fixity) (fixity,[]) oplist}
 
addOpContents :: UserState -> Oper -> OpInstance -> Either AddOpErr UserState
addOpContents stat@UserState{stOp = oplist} op (typelist1,typelist2,sent) = case getOpContents stat op of
 Nothing -> Left Notfound
 Just(fix,list)
  | conflict $ map snd $ toList typelist1 ++ toList typelist2 -> Left Doubleparam
  | otherwise -> do
   newlist <- newlist' 
   return $ stat{stOp = M.insert op (fix,newlist) oplist}
   where 
    newlist' :: Either AddOpErr [OpInstance]
    newlist'   
     | or [ typelist1 `overlaps` tlist1 && typelist2 `overlaps` tlist2 | (tlist1,tlist2,_) <- list ] = Left Doubledefine
     | otherwise = Right $ (typelist1,typelist2,sent):list