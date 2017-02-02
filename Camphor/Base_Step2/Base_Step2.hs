{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
{- Functional macro expansion -}
module Camphor.Base_Step2.Base_Step2
(step2
) where 

import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.SepList(SepList(..)) 
import Camphor.Base_Step2.Type
import Camphor.Base_Step2.UserState
import Camphor.Base_Step2.New 
import Camphor.Base_Step2.Auxilary
import Camphor.Base_Step2.Base_Step2_2(parser2_2)
import Camphor.Base_Step2.PCS_Parser(parser2')
import Camphor.Base_Step2.Replacer2(replacer2)
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import Camphor.Global.Operators
import Camphor.NonEmpty
import Text.Parsec  
 
step2 ::  FilePath -> Txt -> Either ParseError Txt
step2 file txt = do
 xs <- parse parser2'  (file ++ "-step2"  ) txt
 ys <- parse parser2_2 (file ++ "-step2-2") xs
 zs <- convert ys
 return zs
 
defaultStat :: UserState 
defaultStat = emptyState

convert :: Sents -> Either ParseError Txt
convert xs = convert2 defaultStat xs 

{------------------------------------------------------------------------------------- 
 -                              * definition of convert2 *                           -
 -------------------------------------------------------------------------------------}

convert2 :: UserState -> Sents -> Either ParseError Txt
convert2 _    []                         = Right "" 
convert2 stat (Single(_  ,Comm comm):xs) = ("/*"++comm++"*/") <++$> convert2 stat xs
convert2 stat (Single(_  ,Sp   sp  ):xs) = sp                 <++$> convert2 stat xs
convert2 stat (Single(_  ,Scolon   ):xs) = ";"                <++$> convert2 stat xs
convert2 stat (Single(pos,Char iden):xs) = do
 newStat <- newC pos iden stat 
 left <- convert2 newStat  xs
 return ("char " ++ iden ++";"++left)
 
convert2 stat (Single(pos,Del  iden):xs) = do
 newStat <- newD pos iden stat -- FIXME
 left <- convert2 newStat  xs
 return ("delete " ++iden++";"++left)
 
convert2 stat (Single(pos,Infl fixity op):xs) = do
 newStat <- newL pos fixity op stat 
 left <- convert2 newStat xs 
 return left
 
convert2 stat (Single(pos,Infr fixity op):xs) = do
 newStat <- newR pos fixity op stat 
 left <- convert2 newStat xs 
 return left

convert2 stat (Single(pos,Func1 name typelist sent):xs) = do
 newStat <- newF1 pos name typelist sent stat 
 left <- convert2 newStat xs
 return left
 
convert2 stat (Single(pos,Call1 name valuelist):Single(_,Scolon):xs) = do
 result <- newK1 pos name valuelist stat
 left <- convert2 stat xs
 return $ result ++ left
 
convert2 stat (Single(pos,Func2 op typelist1 typelist2 sent):xs) = do
 newStat <- newF2 pos op typelist1 typelist2 sent stat
 left <- convert2 newStat xs
 return left
 
convert2 stat (Block ys:xs) = do
 (newStat,result) <- newB stat ys
 left <- convert2 newStat xs
 return$ result ++ left

convert2 _    (Single(pos,Call1 _ _):[]) = Left$newErrorMessage(UnExpect$"end of input")pos
convert2 _    (Single(pos,Call1 _ _):Single(_,Char v        ):_) = Left$newErrorMessage(UnExpect$"definition of variable "++show v)pos
convert2 _    (Single(pos,Call1 _ _):Single(_,Del  v        ):_) = Left$newErrorMessage(UnExpect$"deletion of variable "  ++show v)pos
convert2 _    (Single(pos,Call1 _ _):Single(_,Infl _ op     ):_) = Left$newErrorMessage(UnExpect$"fixity declaration of operator "++show op)pos
convert2 _    (Single(pos,Call1 _ _):Single(_,Infr _ op     ):_) = Left$newErrorMessage(UnExpect$"fixity declaration of operator "++show op)pos
convert2 _    (Single(pos,Call1 _ _):Single(_,Func1 name _ _):_) = Left$newErrorMessage(UnExpect$"definition of function "++show name)pos
convert2 _    (Single(pos,Call1 _ _):Single(_,Func2 op _ _ _):_) = Left$newErrorMessage(UnExpect$"definition of operator "++show op)pos
convert2 _    (Single(pos,Call1 _ _):Single(_,Call1 name _  ):_) = Left$newErrorMessage(UnExpect$"call of function "++show name)pos
convert2 _    (Single(pos,Call1 _ _):Single(_,Call2 op _ _  ):_) = Left$newErrorMessage(UnExpect$"call of operator "++show op)pos
convert2 _    (Single(pos,Call1 _ _):Single(_,Call3 op _ _  ):_) = Left$newErrorMessage(UnExpect$"call of operator "++show op)pos
convert2 _    (Single(pos,Call1 _ _):Single(_,Call4 _ _     ):_) = Left$newErrorMessage(UnExpect$"call of operator ")pos
convert2 _    (Single(pos,Call1 _ _):Single(_,Call5 _       ):_) = Left$newErrorMessage(UnExpect$"call of operator ")pos

convert2 stat (c@(Single(_,Call1 _ _)):Single(_,Comm _):xs) = convert2 stat (c:xs) 
convert2 stat (c@(Single(_,Call1 _ _)):Single(_,Sp   _):xs) = convert2 stat (c:xs) 

convert2 stat (Single(_,Call1 name valuelist):Block ys:xs) = do -- FIXME: does not replace a function call when it's followed by a block
 left <- convert2 stat (Block ys:xs)
 return$ showCall name valuelist ++ left
  where 
   showCall :: Ident -> ValueList -> String
   showCall nm (SepList(v,ovs)) = nm ++ "(" ++ show' v ++ concat[ (unOp o2) ++ show' v2 | (o2,v2) <- ovs ] ++ ")"
  
--- (val [op val])op(val [op val]);
convert2 stat (Single(pos,Call2 op valuelist1 valuelist2):xs) = do
 result <- newK2 pos op valuelist1 valuelist2 stat
 left <- convert2 stat xs
 return(result ++ left)

--- (val [op val])op val [op val] ; 
convert2 stat (Single(pos,Call3 op  valuelist1 valuelist2):xs) = do
 result <- newK3 pos op valuelist1 valuelist2 stat
 left <- convert2 stat xs
 return(result ++ left)

--- [val op] (val [op val]) ; 
convert2 stat (Single(pos,Call4 list  valuelist):xs) = do
 result <- newK4 pos list valuelist stat
 left <- convert2 stat xs
 return(result ++ left)

--- val [op val] op val [op val] ;
convert2 stat (Single(pos,Call5 valuelist):xs) = do
 result <- newK5 pos valuelist stat
 left <- convert2 stat xs
 return(result ++ left)
{-----------------------------------------------------------
 -                   * end of convert2 *                   -
 -----------------------------------------------------------}
 
newB :: UserState -> Sents -> Either ParseError (UserState,Txt)
newB stat ys = do
 (newStat,res) <- newB2 stat ys
 return(newStat,"{" ++ res ++ "}")

newB2 :: UserState -> Sents -> Either ParseError (UserState,Txt)
newB2 stat ys = do
 txt <- convert2 stat ys -- FIXME : state is not passed
 return(stat,txt)

-- Function call
newK1 :: SourcePos -> Ident -> ValueList -> UserState -> Either ParseError Txt
newK1 pos name valuelist stat = do
 result <- replaceFuncMacro pos name valuelist stat
 return result -- stat is unchanged
   
-- normalized operator call
newK2 :: SourcePos -> Oper -> ValueList -> ValueList -> UserState -> Either ParseError Txt 
newK2 pos op valuelist1 valuelist2 stat = do
 result <- replaceOpMacro pos op valuelist1 valuelist2 stat
 return result -- stat is unchanged
 
-- left-parenthesized operator call
newK3 :: SourcePos -> Oper -> ValueList -> ValueList -> UserState -> Either ParseError Txt 
newK3 pos op valuelist1 valuelist2 stat = do
 isValidCall3 pos op valuelist2 stat
 result   <- replaceOpMacro pos op valuelist1 valuelist2 stat
 return result -- stat is unchanged
  
--- Call4 [(Value,Oper)] ValueList
--- right-parenthesized operator call
newK4 :: SourcePos -> [(Value,Oper)] -> ValueList -> UserState -> Either ParseError Txt
newK4 pos [] valuelist stat = newK5 pos valuelist stat -- (val op val); thus is a Call5
newK4 pos (x:xs) valuelist2 stat = do
 (valuelist1,op) <- getCall4Left pos (x:|xs) stat
 result   <- replaceOpMacro pos op valuelist1 valuelist2 stat
 return result

--- no-parenthesized operator call  
newK5 :: SourcePos -> ValueList -> UserState -> Either ParseError Txt
newK5 _   (SepList(Constant _,[])) _     = return "" -- 123; is a nullary sentence
newK5 pos (SepList(Var ident ,[])) stat  = case getVFContents stat ident of
   Nothing        -> Left $newErrorMessage(Message$"identifier "++show ident++" is not defined")pos 
   Just(Left ())  -> return ""
   Just(Right _)  -> Left $newErrorMessage(Message$"cannot use variable "++show ident++" because it is already defined as a function")pos  

newK5 pos (SepList(x,ov:ovs)) stat = do
 (oper,vlist1,vlist2) <- getCall5Result pos (x,ov:|ovs) stat
 newK2 pos oper vlist1 vlist2 stat

--- macro-replacing function for operator
replaceOpMacro :: SourcePos -> Oper -> ValueList -> ValueList -> UserState -> Either ParseError Txt
replaceOpMacro pos op valuelist1 valuelist2 stat = do
 instnce <- getInstanceOfCall2 pos op valuelist1 valuelist2 stat
 replacerOfOp (Operator op instnce) instnce valuelist1 valuelist2 stat
   
--- macro-replacing function for operator   
replaceFuncMacro :: SourcePos -> Ident -> ValueList -> UserState -> Either ParseError Txt   
replaceFuncMacro pos ident valuelist stat = do
 instnce <- getInstanceOfCall1 pos ident valuelist stat
 replacerOfFunc (Func ident instnce) instnce valuelist stat

replacerOfOp :: MacroId -> (TypeList,TypeList, Sent) -> ValueList -> ValueList -> UserState -> Either ParseError Txt
replacerOfOp opname (typelist1,typelist2,sent) valuelist1 valuelist2 stat = 
 replacer opname sent stat $ makeReplacerTable2 (typelist1,typelist2) (valuelist1,valuelist2)

replacerOfFunc :: MacroId -> (TypeList, Sent) -> ValueList -> UserState -> Either ParseError Txt
replacerOfFunc funcname (typelist,sent) valuelist stat =
 replacer funcname sent stat $ makeReplacerTable typelist valuelist
 
replacer :: MacroId -> Sent -> UserState -> ReplTable -> Either ParseError Txt
replacer mname (Single(pos2,ssent)) stat table = do
 newSents <- replacer2 stat (mname:|[]) pos2 ssent table
 case newSents of newSSent:|[] -> convert2 stat [Single(pos2,newSSent)] ; (x:|xs) -> convert2 stat [Block$map(\k->Single(pos2,k))(x:xs)]
replacer mname (Block xs) stat table = do
 result <- sequence [replacer mname ssent stat table | ssent <- xs]
 return$concat(["{"]++result++["}"])