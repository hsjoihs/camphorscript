{-# LANGUAGE FlexibleContexts , TypeSynonymInstances , FlexibleInstances #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base_Step2.UserState
(emptyState
,containsIdent,addIdent,removeIdent,getVFContents
,addOpFixity,getOpName,containsOp,getOpContents,matches,getFixValue
,isInfixL,isInfixR
,show',PrettyPrint,MacroId(..)
)where
import Camphor.SepList
import Camphor.Base_Step2.Type
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
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
 
-- (Type, Ident, [(Oper, Type, Ident)]) 
instance PrettyPrint TypeList where
 show' (SepList((typ, ident), xs)) = show' typ ++ " " ++ ident ++ " " ++ concatMap (\(o,(t,i)) -> o ++ " " ++ show' t ++ " " ++ i ++ " ") xs
 
instance PrettyPrint Type where
 show' CNSTNT_CHAR = "constant char"
 show' CONST_CHAR = "const char"
 show' CHAR_AND = "char&"
 
instance PrettyPrint MacroId where
 show' (Func ident (typelist,_)) = "function "++ident++"("++show' typelist++"){ .. }"
 show' (Operator oper (typelist1,typelist2,_)) = "operator ("++oper++")("++show' typelist1++";"++show' typelist2++"){ .. }"



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
matches (SepList(val,ovs)) (SepList((typ,_),otis))
 | length ovs /= length otis    = False -- wrong length
 | not(val `isTypeof` typ)      = False -- wrong type
 | all id $ zipMatch ovs otis   = True
 | otherwise                    = False
 where 
  zipMatch :: [(Oper,Value)] -> [(Oper,(Type,a))] -> [Bool]
  zipMatch = zipWith (\(op2,val2)(op3,(typ3,_)) -> remSpace op2 == remSpace op3 && val2 `isTypeof` typ3)

isTypeof :: Value -> Type -> Bool
isTypeof _            CONST_CHAR  = True
isTypeof (Var      _) CHAR_AND    = True
isTypeof (Constant _) CNSTNT_CHAR = True
isTypeof _            _           = False