{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base.Base_Step2.ErrList
(Err(..)
 ,Step2Err(..)
  ,TypeErr(..)
   ,TCErr(..)
   ,TDErr(..)
    ,Kind(..)
    ,Kind2(..)
	,Kind5(..)
	,Kind7(..)
  ,FixErr(..)
   ,FDErr(..)
    ,Kind3(..)
  ,AccErr(..)
   ,ADefErr(..)
   ,ADelErr(..)
   ,ARErr(..)
    ,Kind4(..)
  ,FinErr(..)
   ,Kind6(..)
  ,PragErr(..)
   ,PMErr(..)
    ,PMUErr(..)
,toString,toPE,(<!>)
)where
import Camphor.SafePrelude
import Camphor.Base.Base_Step2.Type
import Camphor.Base.Base_Step2.Auxilary2
import Camphor.Global.Utilities
import Text.Parsec  
import Camphor.Global.Synonyms
import Camphor.NonEmpty

infixr 9 <!>
(<!>) :: (a -> b) -> a -> b
(<!>) = id

data Err = Step2 Step2Err deriving(Show,Eq,Ord)
data Step2Err = Type TypeErr | Fixity FixErr | Access AccErr | Finish FinErr | Prag PragErr deriving(Show,Eq,Ord)
data TypeErr = WrongCall TCErr | WrongDef TDErr deriving(Show,Eq,Ord)
data TCErr = Notypematch Kind | Manytypematches Int Kind | Notdefined Kind | Definedasvar Kind2 | Argoverlap Kind | Nulldefined Kind5 | Definedasarg Kind2 | Leftofbuiltin String Kind7 | Recursivecall MacroId MacroId | Infixconflict Fixity Fixity | Smaller Oper Kind3 deriving(Show,Eq,Ord)
data Kind = Functi Ident2 | Operat Oper | Synt Ident2 deriving(Show,Eq,Ord)
data Kind2 = Functi_2 Ident2 | Synt_2 Ident2 deriving(Show,Eq,Ord)
data Kind5 = Functi_4 Ident2 | Operat_4 Oper deriving(Show,Eq,Ord)
data TDErr = Definedasvar_2 Kind2 | Fixnotdefined Kind3 | Typeoverlap Kind | Paramoverlap Kind | InsideCall Kind deriving(Show,Eq,Ord)
data Kind7 = Variab Ident2 | Consta Integer deriving(Show,Eq,Ord)

data FixErr = WrongDef_2 FDErr deriving(Show,Eq,Ord) -- errors about `infixl' and `infixr' 
data FDErr = Conflictfixdef Kind3 | InsideCall_2 Kind3 deriving(Show,Eq,Ord)
data Kind3 = Operat_3 Oper deriving(Show,Eq,Ord) 
 
data AccErr = WrongDel ADelErr  | WrongDef_3 ADefErr | WrongRef ARErr deriving(Show,Eq,Ord)
data ADefErr = Alreadydefined Kind4 | Definedasarg_3 Kind4 deriving(Show,Eq,Ord)
data ADelErr = Notdefined_2 Kind4 | Notdefinedhere Kind4 | Definedasarg_2 Kind4 deriving(Show,Eq,Ord)
data ARErr = Notdefined_3 Kind4 | Definedasfunsyn Kind4 deriving(Show,Eq,Ord)
data Kind4 = Idn Ident2 deriving(Show,Eq,Ord)

data FinErr = NotDel Kind6 deriving(Show,Eq,Ord)
data Kind6 = Idns (NonEmpty Ident2) deriving(Show,Eq,Ord)

data PragErr = Memory PMErr deriving(Show,Eq,Ord)
data PMErr = Using PMUErr deriving(Show,Eq,Ord)
data PMUErr = NotValidIdent String | IsConstant (NonEmpty Integer) deriving(Show,Eq,Ord) 
 
 
toString :: Err -> String
toString (Step2(Type(WrongCall(Notypematch(Functi i)))))       = "no type-matching instance of function "++showIdent i ++" defined"
toString (Step2(Type(WrongCall(Notypematch(Operat o)))))       = "no type-matching instance of operator "++showStr(unOp o)++" defined"
toString (Step2(Type(WrongCall(Notypematch(Synt   i)))))       = "no type-matching instance of syntax   "++showIdent i ++" defined"
toString (Step2(Type(WrongCall(Manytypematches n(Functi i))))) = showNum n ++" type-matching instances of function "++showIdent i++" defined"
toString (Step2(Type(WrongCall(Manytypematches n(Operat o))))) = showNum n ++" type-matching instances of operator "++showStr(unOp o)++" defined"
toString (Step2(Type(WrongCall(Manytypematches n(Synt   i))))) = showNum n ++" type-matching instances of syntax   "++showIdent i++" defined"
toString (Step2(Type(WrongCall(Notdefined(Functi i)))))        = "function "++showIdent i++" is not defined"
toString (Step2(Type(WrongCall(Notdefined(Operat o)))))        = "operator "++showStr (unOp o)++" is not defined"
toString (Step2(Type(WrongCall(Notdefined(Synt   i)))))        = "syntax   "++showIdent i ++" is not defined"
toString (Step2(Type(WrongCall(Definedasvar(Functi_2 i)))))    = "cannot call "++showIdent i++" as a function because it is defined as a variable"
toString (Step2(Type(WrongCall(Definedasvar(Synt_2   i)))))    = "cannot use " ++showIdent i++" as a syntax because it is defined as a variable"
toString (Step2(Type(WrongCall(Argoverlap(Functi i)))))        = "overlapping arguments of function "++showIdent i
toString (Step2(Type(WrongCall(Argoverlap(Operat o)))))        = "overlapping arguments of operator "++showStr (unOp o)
toString (Step2(Type(WrongCall(Argoverlap(Synt   i)))))        = "overlapping arguments of syntax "++showIdent i
toString (Step2(Type(WrongCall(Nulldefined(Functi_4 i)))))     = "cannot call function "++showIdent i++" because it is defined as null"
toString (Step2(Type(WrongCall(Nulldefined(Operat_4 o)))))     = "cannot call operator "++showStr(unOp o)++" because it is defined as null"
toString (Step2(Type(WrongCall(Definedasarg(Functi_2 i)))))    = "cannot call "++showIdent i++" as a function because it is defined as an argument"
toString (Step2(Type(WrongCall(Definedasarg(Synt_2 i)))))      = "cannot use "++showIdent i++" as a syntax because it is defined as an argument"
toString (Step2(Type(WrongCall(Leftofbuiltin str(Variab c))))) = "cannot have "++showIdent c++" at the left side of operator "++showStr str++"because it is a constant"
toString (Step2(Type(WrongCall(Leftofbuiltin str(Consta c))))) = "cannot have "++showNum c++" at the left side of operator "++showStr str++"because it is a constant"
toString (Step2(Type(WrongCall(Recursivecall n n2))))          = "cannot call "++show' n2 ++" recursively inside "++show' n
toString (Step2(Type(WrongCall(Infixconflict k k2))))          = "cannot mix " ++ show' k ++ " and " ++ show' k2 ++ " in the same infix expression"
toString (Step2(Type(WrongCall(Smaller o2 (Operat_3 o)))))     = "cannot call operator " ++showStr(unOp o)++ " because it has smaller fixity than operator " ++showStr(unOp o2)
 

toString (Step2(Type(WrongDef(Definedasvar_2(Functi_2 i)))))   = "cannot define function " ++ showIdent i ++ " because it is already defined as a variable"
toString (Step2(Type(WrongDef(Definedasvar_2(Synt_2 i)))))     = "cannot define syntax " ++ showIdent i ++ " because it is already defined as a variable"
toString (Step2(Type(WrongDef(Fixnotdefined(Operat_3 o)))))    = "fixity of operator "++showStr (unOp o)++" is not defined"
toString (Step2(Type(WrongDef(Typeoverlap(Functi i)))))        = "type-overlapping definition of function "++showIdent i
toString (Step2(Type(WrongDef(Typeoverlap(Synt i)))))          = "type-overlapping definition of syntax "++showIdent i
toString (Step2(Type(WrongDef(Typeoverlap(Operat o)))))        = "type-overlapping definition of operator "++showStr (unOp o)
toString (Step2(Type(WrongDef(Paramoverlap(Functi i)))))       = "overlapping parameters of function "++showIdent i
toString (Step2(Type(WrongDef(Paramoverlap(Synt i)))))         = "overlapping parameters of syntax "++showIdent i
toString (Step2(Type(WrongDef(Paramoverlap(Operat o)))))       = "overlapping parameters of operator "++showStr (unOp o)
toString (Step2(Type(WrongDef(InsideCall(Functi i)))))         = "cannot define function " ++ showIdent i ++ " inside function/operator definition"
toString (Step2(Type(WrongDef(InsideCall(Synt i)))))           = "cannot define syntax " ++ showIdent i ++ " inside function/operator definition"
toString (Step2(Type(WrongDef(InsideCall(Operat o)))))         = "cannot define operator " ++ showStr (unOp o) ++ " inside function/operator definition"

toString (Step2(Access(WrongDel(Notdefined_2(Idn i)))))        = "cannot delete identifier "++showIdent i++" because it is not defined"
toString (Step2(Access(WrongDel(Notdefinedhere(Idn i)))))      = "cannot delete identifier "++showIdent i++" because it is not defined in this function/operator"
toString (Step2(Access(WrongDel(Definedasarg_2(Idn i)))))      = "cannot delete identifier "++showIdent i++" because it is defined as an argument"
toString (Step2(Access(WrongRef(Notdefined_3(Idn i)))))        = "cannot access variable "  ++showIdent i++" because it is not defined"
toString (Step2(Access(WrongRef(Definedasfunsyn(Idn i)))))     = "cannot access variable "  ++showIdent i++" because it is already defined as a function/syntax"
toString (Step2(Access(WrongDef_3(Alreadydefined(Idn i)))))    = "cannot define identifier "++showIdent i++" because it is already defined"
toString (Step2(Access(WrongDef_3(Definedasarg_3(Idn i)))))    = "cannot define identifier "++showIdent i++" because it is already defined as an argument"

toString (Step2(Fixity(WrongDef_2(Conflictfixdef(Operat_3 o))))) = "conflicting fixity definitions of operator "++showStr (unOp o)
toString (Step2(Fixity(WrongDef_2(InsideCall_2(Operat_3 o)))))   = "cannot declare fixity of "++showStr (unOp o)++" inside function/operator definition "

toString (Step2(Finish(NotDel(Idns is)))) = "cannot finish a block without deleting " ++ pret "identifier" unId is

toString (Step2(Prag(Memory(Using(NotValidIdent str))))) = "cannot use " ++ showStr str ++ " as a valid identifier in a `MEMORY using' pragma"
toString (Step2(Prag(Memory(Using(IsConstant cs))))) = "cannot use " ++ pret "constant" showNum cs ++ " in a `MEMORY using' pragma"

pret :: String -> (a -> String) -> NonEmpty a -> String
pret prefix f (i:|[]) = prefix ++ " " ++ f i
pret prefix f (i:|is) = prefix ++ "s " ++ f i ++ concatMap ((", "++) . f)is



toPE :: SourcePos -> Err -> ParseError
toPE pos er  = newErrorMessage(Message$toString er)pos