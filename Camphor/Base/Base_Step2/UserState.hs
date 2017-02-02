{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.Base.Base_Step2.UserState
(OpInfo,MacroId(..),VFInstance,OpInstance
,emptyState,UserState(),TmpStat
,getTmp,setTmp,clearTmp
,containsIdent,addIdent,removeIdent,getVFContents,addOpContents,containsAnyIdent
,addOpFixity,getOpName,containsOp,getOpContents,matches,getFixValue
,isInfixL,isInfixR
,show',PrettyPrint
,addVFBlock,getTopVFBlock,deleteTopVFBlock,deleteTopVFBlock_ 
,overlaps,typelistIdentConflict,valuelistIdentConflict
,getName
)where
import Camphor.SafePrelude
import Camphor.SepList as Sep
import Camphor.Base.Base_Step2.Type
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import qualified Data.Map as M
import Text.Parsec.Pos(newPos)
import Camphor.NonEmpty as NE
import Control.Monad.State hiding(fix)

type VFInstance = (TypeList, Maybe Sent) -- Maybe Sent ::: block or `null function'
type OpInstance = (TypeList,TypeList, Maybe Sent2)
type OpInfo = (Fixity,[OpInstance])
data MacroId = Func Ident2 VFInstance | Operator Oper OpInstance deriving(Show,Eq)  
-- private
type VFList = NonEmpty(M.Map Ident2 VFInfo)
type OpList = M.Map Oper OpInfo
data UserState = UserState VFList OpList (Maybe TmpStat) deriving(Show)
type VFInfo = Between () [VFInstance]

getName :: MacroId -> String
getName (Func ident _) = showIdent ident
getName (Operator oper _ ) = unOp oper

overlaps :: TypeList -> TypeList -> Bool
overlaps (SepList (typ,_) xs) (SepList (typ2,_) xs2) 
 | typ `clashesWith` typ2 = length xs == length xs2 && all id (zipWith transform xs xs2)
 | otherwise              = False
 where
  transform :: (Oper,(Type,Ident2)) -> (Oper,(Type,Ident2)) -> Bool
  transform (op3,(typ3,_)) (op4,(typ4,_)) = op3 == op4 && typ3 `clashesWith` typ4
  CNSTNT_CHAR `clashesWith` CHAR_AND      = False
  CHAR_AND    `clashesWith` CNSTNT_CHAR   = False 
  _           `clashesWith` _             = True

typelistIdentConflict :: TypeList -> Bool
typelistIdentConflict = conflict . map snd . toList'

valuelistIdentConflict :: ValueList -> Bool
valuelistIdentConflict = conflict . filter isVar . toList'
  
isInfixL :: Fixity -> Bool
isInfixL (InfixL _ _) = True
isInfixL (InfixR _ _) = False

isInfixR :: Fixity -> Bool
isInfixR = not . isInfixL


class PrettyPrint a where
 show' :: a -> String

instance PrettyPrint Fixity where
 show' (InfixL int op) = showStr(unOp op)++"[infixl "++showNum int++"]"
 show' (InfixR int op) = showStr(unOp op)++"[infixr "++showNum int++"]"
 
instance PrettyPrint Value where
 show'(Var x) = unId x
 show'(Constant n) = showNum n
 
-- (Type, Ident2, [(Oper, Type, Ident2)]) 
instance PrettyPrint TypeList where
 show' (SepList (typ, ident) xs) = show' typ ++ " " ++ unId ident ++ " " ++ concatMap (\(o,(t,i)) -> unOp o ++ " " ++ show' t ++ " " ++ unId i ++ " ") xs
 
instance PrettyPrint Type where
 show' CNSTNT_CHAR = "constant char"
 show' CONST_CHAR = "const char"
 show' CHAR_AND = "char&"
 
instance PrettyPrint MacroId where
 show' (Func ident (typelist,_)) = "function "++unId ident++"("++show' typelist++"){ .. }"
 show' (Operator oper (typelist1,typelist2,_)) = "operator ("++unOp oper++")("++show' typelist1++";"++show' typelist2++"){ .. }"



getFixValue :: Fixity -> Integer
getFixValue (InfixL fix _) = fix
getFixValue (InfixR fix _) = fix

getOpName :: Fixity -> Oper
getOpName (InfixL _ op) = op
getOpName (InfixR _ op) = op
emptyState :: UserState
emptyState = UserState deffun defop Nothing
 where
  deffun :: VFList
  deffun = nE(M.fromList[
   (readI ,West$[(return(CHAR_AND,bbbb),Just$Single(newPos "__DEFAULT__" 0 0) (Rd  bbbb) )]),
   (writeI,West$[(return(CHAR_AND,bbbb),Just$Single(newPos "__DEFAULT__" 0 0) (Wrt bbbb) )])
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

addIdent :: UserState -> Ident2 -> VFInfo -> UserState
addIdent      (UserState (vf:|vfs) oplist tmp) ident dat = UserState ((M.insert ident dat vf):|vfs) oplist tmp

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

 
containsOp :: UserState -> Oper -> Bool
containsOp (UserState _ oplist _) oper = oper `M.member` oplist

getOpContents :: UserState -> Oper -> Maybe OpInfo
getOpContents (UserState _ oplist _) oper = M.lookup oper oplist


matches :: ValueList -> TypeList -> Bool
matches (SepList val ovs) (SepList (typ,_) otis)
 | length ovs /= length otis    = False -- wrong length
 | not(val `isTypeof` typ)      = False -- wrong type
 | all id $ zipMatch ovs otis   = True
 | otherwise                    = False
 where 
  zipMatch :: [(Oper,Value)] -> [(Oper,(Type,a))] -> [Bool]
  zipMatch = zipWith (\(op2,val2)(op3,(typ3,_)) -> op2 == op3 && val2 `isTypeof` typ3)

isTypeof :: Value -> Type -> Bool
isTypeof _            CONST_CHAR  = True
isTypeof (Var      _) CHAR_AND    = True
isTypeof (Constant _) CNSTNT_CHAR = True
isTypeof _            _           = False