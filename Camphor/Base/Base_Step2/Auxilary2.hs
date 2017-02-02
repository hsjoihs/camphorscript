{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base.Base_Step2.Auxilary2
(MacroId(..),VFInstance,OpInstance,SyntaxInstance
-- does not access private members
,isInfixL,isInfixR
,typelistIdentConflict
,valuelistIdentConflict
,getName,getOpName
,getFixValue
,matches,matches2
,overlaps,overlaps'
,PrettyPrint,show'
)where
import Camphor.SafePrelude
import Camphor.SepList as Sep
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import Camphor.NonEmpty as NE
import Camphor.Listlike
import Camphor.TailSepList
import Camphor.Base.Base_Step2.Type



data MacroId = Func Ident2 VFInstance | Operator Oper OpInstance | Syn Ident2 SyntaxInstance deriving(Show,Eq,Ord)  
type VFInstance = (TypeList, Maybe Sent) -- Maybe Sent ::: block or `null function'
type OpInstance = (TypeList,TypeList, Maybe Sent2)
type SyntaxInstance = (Between TailTypeList TypeList,Sent) -- list, block

isInfixL :: Fixity -> Bool
isInfixL (InfixL _ _) = True
isInfixL (InfixR _ _) = False

isInfixR :: Fixity -> Bool
isInfixR = not . isInfixL

typelistIdentConflict :: (Listlike f) => f (Type,Ident2) -> Bool
typelistIdentConflict = conflict . map snd . toList'

valuelistIdentConflict :: (Listlike f) => f Value -> Bool
valuelistIdentConflict = conflict . filter isVar . toList'
  

getName :: MacroId -> String
getName (Func ident _) = showIdent ident
getName (Syn ident _) = showIdent ident
getName (Operator oper _ ) = unOp oper


getOpName :: Fixity -> Oper
getOpName (InfixL _ op) = op
getOpName (InfixR _ op) = op

getFixValue :: Fixity -> Integer 
getFixValue (InfixL fix _) = fix
getFixValue (InfixR fix _) = fix

matches :: ValueList -> TypeList -> Bool
matches (SepList val ovs) (SepList (typ,_) otis)
 | length ovs /= length otis    = False -- wrong length
 | not(val `isTypeof` typ)      = False -- wrong type
 | and $ zipMatch ovs otis   = True
 | otherwise                    = False
  
matches2 :: TailValueList -> TailTypeList -> Bool
matches2 tvl ttl
 | length' tvl /= length' ttl                = False -- wrong length
 | and $ zipMatch (unTSL tvl) (unTSL ttl) = True
 | otherwise                                 = False 
 
zipMatch :: [(Oper,Value)] -> [(Oper,(Type,a))] -> [Bool]
zipMatch = zipWith (\(op2,val2)(op3,(typ3,_)) -> op2 == op3 && val2 `isTypeof` typ3)


isTypeof :: Value -> Type -> Bool
isTypeof _            CONST_CHAR  = True
isTypeof (Var      _) CHAR_AND    = True
isTypeof (Constant _) CNSTNT_CHAR = True
isTypeof _            _           = False

transform :: (Oper,(Type,Ident2)) -> (Oper,(Type,Ident2)) -> Bool
transform (op3,(typ3,_)) (op4,(typ4,_)) = op3 == op4 && typ3 `clashesWith` typ4

clashesWith :: Type -> Type -> Bool
CNSTNT_CHAR `clashesWith` CHAR_AND      = False
CHAR_AND    `clashesWith` CNSTNT_CHAR   = False
_           `clashesWith` _             = True

overlaps :: TypeList -> TypeList -> Bool
overlaps (SepList (typ,_) xs) (SepList (typ2,_) xs2) 
 | typ `clashesWith` typ2 = length xs == length xs2 && and (zipWith transform xs xs2)
 | otherwise              = False
  
overlaps' :: TailTypeList -> TailTypeList -> Bool
overlaps' xs xs2 = length' xs == length' xs2 && and (zipWith transform (unTSL xs) (unTSL xs2))




class PrettyPrint a where
 show' :: a -> String

instance PrettyPrint Fixity where
 show' (InfixL int op) = showStr(unOp op)++" [infixl "++showNum int++"]"
 show' (InfixR int op) = showStr(unOp op)++" [infixr "++showNum int++"]"
 
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
 show' (Syn ident (West tl,  _))  = "syntax " ++ unId ident ++ "(" ++ show' tl ++ "){block}{ .. }"
 show' (Syn ident (East ttl, _)) = "syntax " ++ unId ident ++ "(" ++ show' ttl ++ "){block}{ .. }" 

instance PrettyPrint TailTypeList where
 show' (TSL ttl) = concatMap (\(o,(t,i)) -> unOp o ++ " " ++ show' t ++ " " ++ unId i ++ " ") ttl

