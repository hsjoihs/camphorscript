{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base.Base_Step2.ErrList
(Err(..),Step2Err(..),TypeErr(..),TCErr(..),Kind(..),toString,toPE,(<!>),Kind2(..)
)where
import Camphor.SafePrelude
import Camphor.Base.Base_Step2.Type
import Camphor.Global.Utilities
import Text.Parsec  
import Camphor.Global.Synonyms



infixr 9 <!>
(<!>) :: (a -> b) -> a -> b
(<!>) = id

data Err = Step2 Step2Err deriving(Show,Eq,Ord)
data Step2Err = Type TypeErr deriving(Show,Eq,Ord)
data TypeErr = WrongCall TCErr deriving(Show,Eq,Ord)
data TCErr = Notypematch Kind | Manytypematches Int Kind | Notdefined Kind | Definedasvar Kind2 deriving(Show,Eq,Ord)
data Kind = Functi Ident2 | Operat Oper | Synt Ident2 deriving(Show,Eq,Ord)
data Kind2 = Functi_2 Ident2 | Synt_2 Ident2 deriving(Show,Eq,Ord)
 

toString :: Err -> String
toString (Step2(Type(WrongCall(Notypematch(Functi i)))))       = "no type-matching instance of function "++showIdent i ++" defined"
toString (Step2(Type(WrongCall(Notypematch(Operat o)))))       = "no type-matching instance of operator "++showStr(unOp o)++" defined"
toString (Step2(Type(WrongCall(Notypematch(Synt   i)))))       = "no type-matching instance of syntax   "++showIdent i ++" defined"
toString (Step2(Type(WrongCall(Manytypematches n(Functi i))))) = showNum n ++" type-matching instances of function "++showIdent i++" defined"
toString (Step2(Type(WrongCall(Manytypematches n(Operat o))))) = showNum n ++" type-matching instances of operator "++showStr(unOp o)++" defined"
toString (Step2(Type(WrongCall(Manytypematches n(Synt   i))))) = showNum n ++" type-matching instances of syntax   "++showIdent i++" defined"
toString (Step2(Type(WrongCall(Notdefined(Functi i)))))        = "function "++showIdent i++" is not defined"
toString (Step2(Type(WrongCall(Notdefined(Operat o)))))        = "operator "++showStr (unOp o)++" is not defined"
toString (Step2(Type(WrongCall(Notdefined(Synt   i)))))        = "no type-matching instance of syntax   "++showIdent i ++" defined"
toString (Step2(Type(WrongCall(Definedasvar(Functi_2 i)))))    = "cannot call function "++showIdent i++" because it is defined as a variable"
toString (Step2(Type(WrongCall(Definedasvar(Synt_2   i)))))    = "cannot use syntax "   ++showIdent i++" because it is defined as a variable"

toPE :: SourcePos -> Err -> ParseError
toPE pos er  = newErrorMessage(Message$toString er)pos