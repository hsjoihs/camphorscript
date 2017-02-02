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
  ,ImposErr(..)
,toString,toPE,(<!>),toPath
)where
import Camphor.SafePrelude
import Camphor.Show
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
data Step2Err = Type TypeErr | Fixity FixErr | Access AccErr | Finish FinErr | Prag PragErr | Impossible ImposErr deriving(Show,Eq,Ord)
data TypeErr = WrongCall TCErr | WrongDef TDErr deriving(Show,Eq,Ord)
data TCErr = Notypematch Kind | Manytypematches Int Kind | Notdefined Kind | Definedasvar Kind2 | Argoverlap Kind | Nulldefined Kind5 | Definedasarg Kind2 | Leftofbuiltin String Kind7 | Recursivecall MacroId MacroId | Infixconflict Fixity Fixity | Fixnotdefined_2 Kind3 | Smaller Oper Kind3 deriving(Show,Eq,Ord)
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

data ImposErr = Integerranout deriving(Show,Eq,Ord) 
toString :: Err -> String
toString = snd . t

qwer :: String -> (Int,String)
qwer = (,)0
 
t :: Err -> (Int,String)
t(Step2(Type(WrongCall(Notypematch(Functi i)))))       = (1006,"no type-matching instance of function "++showIdent i ++" defined")
t(Step2(Type(WrongCall(Notypematch(Operat o)))))       = (1007,"no type-matching instance of operator "++showStr(unOp o)++" defined")
t(Step2(Type(WrongCall(Notypematch(Synt   i)))))       = (1024,"no type-matching instance of syntax "  ++showIdent i ++" defined")
t(Step2(Type(WrongCall(Manytypematches n(Functi i))))) = qwer$ showNum n ++" type-matching instances of function "++showIdent i++" defined"
t(Step2(Type(WrongCall(Manytypematches n(Operat o))))) = qwer$ showNum n ++" type-matching instances of operator "++showStr(unOp o)++" defined"
t(Step2(Type(WrongCall(Manytypematches n(Synt   i))))) = qwer$ showNum n ++" type-matching instances of syntax   "++showIdent i++" defined"
t(Step2(Type(WrongCall(Notdefined(Functi i)))))        = (1008,"cannot call function "++showIdent i++" because it is not defined")
t(Step2(Type(WrongCall(Notdefined(Operat o)))))        = (1009,"cannot call operator "++showStr (unOp o)++" because it is not defined")
t(Step2(Type(WrongCall(Notdefined(Synt   i)))))        = (1025,"cannot use syntax "++showIdent i ++" because it is not defined")
t(Step2(Type(WrongCall(Definedasvar(Functi_2 i)))))    = (1000,"cannot call "++showIdent i++" as a function because it is defined as a variable")
t(Step2(Type(WrongCall(Definedasvar(Synt_2   i)))))    = (1026,"cannot use " ++showIdent i++" as a syntax because it is defined as a variable")
t(Step2(Type(WrongCall(Argoverlap(Functi i)))))        = (1012,"overlapping arguments of function "++showIdent i)
t(Step2(Type(WrongCall(Argoverlap(Operat o)))))        = (1013,"overlapping arguments of operator "++showStr (unOp o))
t(Step2(Type(WrongCall(Argoverlap(Synt   i)))))        = (1027,"overlapping arguments of syntax "++showIdent i)
t(Step2(Type(WrongCall(Nulldefined(Functi_4 i)))))     = (1028,"cannot call function "++showIdent i++" because it is defined as null")
t(Step2(Type(WrongCall(Nulldefined(Operat_4 o)))))     = (1010,"cannot call operator "++showStr(unOp o)++" because it is defined as null")
t(Step2(Type(WrongCall(Definedasarg(Functi_2 i)))))    = (1036,"cannot call "++showIdent i++" as a function because it is defined as an argument")
t(Step2(Type(WrongCall(Definedasarg(Synt_2 i)))))      = (1037,"cannot use "++showIdent i++" as a syntax because it is defined as an argument")
t(Step2(Type(WrongCall(Leftofbuiltin str(Variab c))))) = qwer$ "cannot have "++showIdent c++" at the left side of operator "++showStr str++"because it is a constant"
t(Step2(Type(WrongCall(Leftofbuiltin str(Consta c))))) = qwer$ "cannot have "++showNum c++" at the left side of operator "++showStr str++"because it is a constant"
t(Step2(Type(WrongCall(Recursivecall n n2))))          = (1038,"cannot call "++show' n2 ++" recursively inside "++show' n)
t(Step2(Type(WrongCall(Infixconflict k k2))))          = (1011,"cannot mix " ++ show' k ++ " and " ++ show' k2 ++ " in the same infix expression")
t(Step2(Type(WrongCall(Fixnotdefined_2(Operat_3 o))))) = (1039,"cannot call operator " ++showStr(unOp o)++ " because its fixity is not defined")
t(Step2(Type(WrongCall(Smaller o2 (Operat_3 o)))))     = (1019,"cannot call operator " ++showStr(unOp o2)++ " because it has smaller fixity than operator " ++showStr(unOp o))

t(Step2(Type(WrongDef(Definedasvar_2(Functi_2 i)))))   = (1003,"cannot define function " ++ showIdent i ++ " because it is already defined as a variable")
t(Step2(Type(WrongDef(Definedasvar_2(Synt_2 i)))))     = (1029,"cannot define syntax " ++ showIdent i ++ " because it is already defined as a variable")
t(Step2(Type(WrongDef(Fixnotdefined(Operat_3 o)))))    = (1005,"cannot define operator "++showStr (unOp o)++" because its fixity is not defined")
t(Step2(Type(WrongDef(Typeoverlap(Functi i)))))        = (1020,"type-overlapping definition of function "++showIdent i)
t(Step2(Type(WrongDef(Typeoverlap(Synt i)))))          = (1030,"type-overlapping definition of syntax "++showIdent i)
t(Step2(Type(WrongDef(Typeoverlap(Operat o)))))        = (1021,"type-overlapping definition of operator "++showStr (unOp o))
t(Step2(Type(WrongDef(Paramoverlap(Functi i)))))       = (1014,"overlapping parameters of function "++showIdent i)
t(Step2(Type(WrongDef(Paramoverlap(Synt i)))))         = (1031,"overlapping parameters of syntax "++showIdent i)
t(Step2(Type(WrongDef(Paramoverlap(Operat o)))))       = (1015,"overlapping parameters of operator "++showStr (unOp o))
t(Step2(Type(WrongDef(InsideCall(Functi i)))))         = (1040,"cannot define function " ++ showIdent i ++ " inside function/operator definition")
t(Step2(Type(WrongDef(InsideCall(Synt i)))))           = (1041,"cannot define syntax " ++ showIdent i ++ " inside function/operator definition")
t(Step2(Type(WrongDef(InsideCall(Operat o)))))         = (1042,"cannot define operator " ++ showStr (unOp o) ++ " inside function/operator definition")

t(Step2(Access(WrongDel(Notdefined_2(Idn i)))))        = (1004,"cannot delete identifier "++showIdent i++" because it is not defined")
t(Step2(Access(WrongDel(Notdefinedhere(Idn i)))))      = (1032,"cannot delete identifier "++showIdent i++" because it is not defined in this function/operator")
t(Step2(Access(WrongDel(Definedasarg_2(Idn i)))))      = (1033,"cannot delete identifier "++showIdent i++" because it is defined as an argument")
t(Step2(Access(WrongRef(Notdefined_3(Idn i)))))        = (1018,"cannot access variable "  ++showIdent i++" because it is not defined")
t(Step2(Access(WrongRef(Definedasfunsyn(Idn i)))))     = (1017,"cannot access variable "  ++showIdent i++" because it is already defined as a function/syntax")
t(Step2(Access(WrongDef_3(Alreadydefined(Idn i)))))    = (1022,"cannot define identifier "++showIdent i++" because it is already defined")
t(Step2(Access(WrongDef_3(Definedasarg_3(Idn i)))))    = (1016,"cannot define identifier "++showIdent i++" because it is already defined as an argument")

t(Step2(Fixity(WrongDef_2(Conflictfixdef(Operat_3 o))))) = (1001,"conflicting fixity definitions of operator "++showStr (unOp o))
t(Step2(Fixity(WrongDef_2(InsideCall_2(Operat_3 o)))))   = (1002,"cannot declare fixity of "++showStr (unOp o)++" inside function/operator definition")

t(Step2(Finish(NotDel(Idns is)))) = (1023,"cannot finish a block without deleting " ++ pret "identifier" unId is)

t(Step2(Prag(Memory(Using(NotValidIdent str))))) = (1034,"cannot use " ++ showStr str ++ " as a valid identifier in a `MEMORY using' pragma")
t(Step2(Prag(Memory(Using(IsConstant cs)))))     = (1035,"cannot use " ++ pret "constant" showNum cs ++ " in a `MEMORY using' pragma")

t(Step2(Impossible(Integerranout))) = qwer$ "cannot allocate a temporary variable because integers ran out for a mysterical reason"

toPath :: Err -> String     -- step2/type/wrongdef/definedasvar/functi 
toPath er = clean $ show er
 where
  clean = reverse . drop 1 . dropWhile (/='/') . reverse . handleBar . repl " (" "/" . map toLower
  repl :: String -> String -> String -> String
  repl _ _ [] = ""
  repl orig new str@(x:xs)
   | take(length orig) str == orig = new ++ repl orig new (drop(length orig) str)
   | otherwise = x: repl orig new xs
  handleBar :: String -> String
  handleBar = h False 
  h :: Bool -> String -> String
  h _     []         = ""
  h False ('_':xs)   = h True xs  
  h False (x:xs)     = x:h False xs
  h True  xs@('/':_) = h False xs 
  h True  (_:xs)     = h True xs 
 
 
pret :: String -> (a -> String) -> NonEmpty a -> String
pret prefix f (i:|[]) = prefix ++ " " ++ f i
pret prefix f (i:|is) = prefix ++ "s " ++ f i ++ concatMap ((", "++) . f)is
 

toPE :: SourcePos -> Err -> ParseError
toPE pos er  = newErrorMessage(Message$toString er)pos