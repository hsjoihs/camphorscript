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
import Text.Parsec.Pos(newPos)
import qualified Data.Map as M
 
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

convert2 :: UserState -> Sents -> Either ParseError Txt
convert2 stat sents = snd <$> convert2_2 stat sents

{------------------------------------------------------------------------------------- 
 -                              * definition of convert2_2 *                           -
 -------------------------------------------------------------------------------------}

convert2_2 :: UserState -> Sents -> Either ParseError (UserState,Txt)
convert2_2 stat []                         = Right (stat,"") 
convert2_2 stat (Single(_  ,Comm comm):xs) = ("/*" ++ comm ++ "*/") <++$$> convert2_2 stat xs
convert2_2 stat (Single(_  ,Sp   sp  ):xs) = sp                     <++$$> convert2_2 stat xs
convert2_2 stat (Single(_  ,Scolon   ):xs) = ";"                    <++$$> convert2_2 stat xs
convert2_2 stat (Single(_  ,Pleq (Var ident) (Constant integer)):xs) = (ident ++ "+=" ++ show integer ++ ";") <++$$> convert2_2 stat xs
convert2_2 stat (Single(_  ,Mneq (Var ident) (Constant integer)):xs) = (ident ++ "-=" ++ show integer ++ ";") <++$$> convert2_2 stat xs
convert2_2 stat (Single(_  ,Rd   (Var idnt)):xs) = ("read("  ++ idnt ++ ")" ++ ";") <++$$> convert2_2 stat xs
convert2_2 stat (Single(_  ,Wrt  (Var idnt)):xs) = ("write(" ++ idnt ++ ")" ++ ";") <++$$> convert2_2 stat xs
convert2_2 _ (Single(_  ,Pleq _ _):_) = Left $newErrorMessage(Message$"FIXME:: code 0002")(newPos "__FIXME__" 0 0) 
convert2_2 _ (Single(_  ,Mneq _ _):_) = Left $newErrorMessage(Message$"FIXME:: code 0002")(newPos "__FIXME__" 0 0) 
convert2_2 _ (Single(_  ,Rd  _ ):_   )= Left $newErrorMessage(Message$"FIXME:: code 0002")(newPos "__FIXME__" 0 0) 
convert2_2 _ (Single(_  ,Wrt  _ ):_ ) = Left $newErrorMessage(Message$"FIXME:: code 0002")(newPos "__FIXME__" 0 0) 

convert2_2 stat (Single(pos,Char iden):xs) = do
 newStat <- newC pos iden stat 
 (newStat2,left) <- convert2_2 newStat xs
 return (newStat2,"char " ++ iden ++ ";" ++ left)
 
convert2_2 stat (Single(pos,Del  iden):xs) = do
 newStat <- newD pos iden stat 
 (newStat2,left) <- convert2_2 newStat  xs
 return (newStat2,"delete " ++ iden ++ ";" ++ left)
 
convert2_2 stat (Single(pos,Infl fixity op):xs) = do
 newStat <- newL pos fixity op stat 
 convert2_2 newStat xs 
 
convert2_2 stat (Single(pos,Infr fixity op):xs) = do
 newStat <- newR pos fixity op stat 
 convert2_2 newStat xs 

convert2_2 stat (Single(pos,Func1 name typelist sent):xs) = do
 newStat <- newF1 pos name typelist sent stat 
 convert2_2 newStat xs
 
convert2_2 stat (Single(pos,Call1 name valuelist):xs) = do
 result <- newK1 pos name valuelist stat
 (newStat2,left) <- convert2_2 stat xs
 return (newStat2,result ++ left)
 
convert2_2 stat (Single(pos,Func2 op typelist1 typelist2 sent):xs) = do
 newStat <- newF2 pos op typelist1 typelist2 sent stat
 convert2_2 newStat xs
 
convert2_2 stat (Block ys:xs) = do
 (newStat,result) <- newB (addVFBlock stat) ys -- FIXME: does not check deletion
 let remainingVars = getTopVFBlock newStat
 --let log = show remainingVars
 if not$M.null remainingVars then Left$newErrorMessage(Message$"identifiers not deleted"++show remainingVars)pos else do
 (newStat2,left) <- convert2_2 newStat xs
 return(newStat2,{-log ++ -} result ++ left)
 where pos = newPos "__FIXME__" 0 0



convert2_2 stat (Single(_,Call1WithBlock name valuelist block):xs) = do -- FIXME: does not replace a function call when it's followed by a block
 (newStat2,left) <- convert2_2 stat (Block block :xs)
 return(newStat2,showCall name valuelist ++ left)
  where 
   showCall :: Ident -> ValueList -> String
   showCall nm (SepList(v,ovs)) = nm ++ "(" ++ show' v ++ concat[ (unOp o2) ++ show' v2 | (o2,v2) <- ovs ] ++ ")"
  
--- (val [op val])op(val [op val]);
convert2_2 stat (Single(pos,Call2 op valuelist1 valuelist2):xs) = do
 result <- newK2 pos op valuelist1 valuelist2 stat
 (newStat2,left) <- convert2_2 stat xs
 return(newStat2,result ++ left)

--- (val [op val])op val [op val] ; 
convert2_2 stat (Single(pos,Call3 op  valuelist1 valuelist2):xs) = do
 result <- newK3 pos op valuelist1 valuelist2 stat
 (newStat2,left) <- convert2_2 stat xs
 return(newStat2,result ++ left)

--- [val op] (val [op val]) ; 
convert2_2 stat (Single(pos,Call4 list  valuelist):xs) = do
 result <- newK4 pos list valuelist stat
 (newStat2,left) <- convert2_2 stat xs
 return(newStat2,result ++ left)

--- val [op val] op val [op val] ;
convert2_2 stat (Single(pos,Call5 valuelist):xs) = do
 result <- newK5 pos valuelist stat
 (newStat2,left) <- convert2_2 stat xs
 return(newStat2,result ++ left)
{-----------------------------------------------------------
 -                   * end of convert2_2 *                   -
 -----------------------------------------------------------}
 
newB :: UserState -> Sents -> Either ParseError (UserState,Txt)
newB stat ys = do
 (newStat,res) <- newB2 stat ys
 return(newStat,"{" ++ res ++ "}")

newB2 :: UserState -> Sents -> Either ParseError (UserState,Txt)
newB2 stat ys = do
 (newStat2,txt) <- convert2_2 stat ys 
 return(newStat2,txt)

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