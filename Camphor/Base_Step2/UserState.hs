{-# LANGUAGE FlexibleContexts , TypeSynonymInstances , FlexibleInstances #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base_Step2.UserState
(Fixity(..),OpInfo,MacroId(..)
,emptyState,UserState()
,containsIdent,addIdent,removeIdent,getVFContents,addOpContents
,addOpFixity,getOpName,containsOp,getOpContents,matches,getFixValue
,isInfixL,isInfixR
,show',PrettyPrint
,addVFBlock,getTopVFBlock
)where
import Camphor.SepList
import Camphor.Base_Step2.Type
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Global.Synonyms
import qualified Data.Map as M
import Text.Parsec.Pos(newPos)
import Camphor.NonEmpty as NE

data Fixity = InfixL Integer Oper | InfixR Integer Oper deriving(Show,Eq) 
type VFInfo = Either () [(TypeList, Sent)]
type OpInfo = (Fixity,[(TypeList,TypeList, Sent)])
data MacroId = Func Ident (TypeList,Sent) | Operator Oper (TypeList,TypeList,Sent) deriving(Show,Eq)  
type VFList = NonEmpty(M.Map Ident VFInfo)
type OpList = M.Map Oper OpInfo
data UserState = UserState VFList OpList


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
 
-- (Type, Ident, [(Oper, Type, Ident)]) 
instance PrettyPrint TypeList where
 show' (SepList((typ, ident), xs)) = show' typ ++ " " ++ ident ++ " " ++ concatMap (\(o,(t,i)) -> unOp o ++ " " ++ show' t ++ " " ++ i ++ " ") xs
 
instance PrettyPrint Type where
 show' CNSTNT_CHAR = "constant char"
 show' CONST_CHAR = "const char"
 show' CHAR_AND = "char&"
 
instance PrettyPrint MacroId where
 show' (Func ident (typelist,_)) = "function "++ident++"("++show' typelist++"){ .. }"
 show' (Operator oper (typelist1,typelist2,_)) = "operator ("++unOp oper++")("++show' typelist1++";"++show' typelist2++"){ .. }"



getFixValue :: Fixity -> Integer
getFixValue (InfixL fix _) = fix
getFixValue (InfixR fix _) = fix

getOpName :: Fixity -> Oper
getOpName (InfixL _ op) = op
getOpName (InfixR _ op) = op

-- FIXME:: a hack using Sp to represent a non-space string
emptyState :: UserState
emptyState = UserState deffun defop
 where
  deffun :: VFList
  deffun = (M.fromList[
   ("read" ,Right[(single CHAR_AND "bbbb",Single(newPos "__DEFAULT__" 0 0,Rd (Var "bbbb")))]),
   ("write",Right[(single CHAR_AND "bbbb",Single(newPos "__DEFAULT__" 0 0,Wrt(Var "bbbb")))])
   ]):|[]
  defop :: OpList
  defop = M.fromList[defau "+="$Pleq (Var "aaaa") (Var "NNNN") ,defau "-="$Mneq(Var "aaaa")(Var "NNNN")]
  defau :: String -> SimpleSent -> (Oper,OpInfo)
  defau a s = (wrap a,(InfixR 5 (wrap a),[(single CHAR_AND "aaaa",single CNSTNT_CHAR "NNNN", Single(newPos "__DEFAULT__" 0 0,s))]))
  single :: Type -> Ident -> TypeList
  single a b = SepList ((a,b),[])

-- checks if the current scope already defines the variable; thus it does not search deeply  
containsIdent :: UserState -> Ident -> Bool
containsIdent (UserState (vf:|_) _) ident = ident `M.member` vf 

addVFBlock :: UserState -> UserState
addVFBlock (UserState vflist oplist) = UserState (M.empty `cons` vflist) oplist

getTopVFBlock :: UserState -> M.Map Ident VFInfo
getTopVFBlock (UserState (vf:|_) _) = vf

addIdent :: UserState -> Ident -> VFInfo -> UserState
addIdent      (UserState (vf:|vfs) oplist) ident dat = UserState ((M.insert ident dat vf):|vfs) oplist 

getVFContents :: UserState -> Ident -> Maybe VFInfo
getVFContents (UserState vflist _) ident = searchBy (M.lookup ident) vflist

removeIdent :: UserState -> Ident -> UserState
removeIdent (UserState vflist oplist) ident = case vflist of 
 vf:|[]        -> UserState (M.delete ident vf:|[]) oplist
 vf:|(vf2:vfs) -> case M.lookup ident vf of
  Nothing -> removeIdent (UserState (vf2:|vfs) oplist) ident
  _       -> UserState (M.delete ident vf:|[]) oplist

addOpFixity :: UserState -> Fixity -> UserState
addOpFixity (UserState vflist oplist) fixity = 
 UserState vflist (M.insert (getOpName fixity) (fixity,[]) oplist)
 
addOpContents :: UserState -> Oper -> (TypeList,TypeList,Sent) -> Maybe UserState
addOpContents stat@(UserState vflist oplist) op (typelist1,typelist2,sent) = case getOpContents stat op of
 Nothing -> Nothing
 Just(fix,list) -> Just$UserState vflist (M.insert op (fix,newlist) oplist)
  where 
   newlist = (typelist1,typelist2,sent):list -- FIXME : does not check the double definition

 
containsOp :: UserState -> Oper -> Bool
containsOp (UserState _ oplist) oper = oper `M.member` oplist

getOpContents :: UserState -> Oper -> Maybe OpInfo
getOpContents (UserState _ oplist) oper = M.lookup oper oplist


matches :: ValueList -> TypeList -> Bool
matches (SepList(val,ovs)) (SepList((typ,_),otis))
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