{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
{- Functional macro expansion -}
module Camphor.Base.Base_Step2.Base_Step2
(step2
) where 

import Camphor.SafePrelude
import Camphor.SepList as Sep
import Camphor.Base.Base_Step2.Type
import Camphor.Base.Base_Step2.UserState
import Camphor.Base.Base_Step2.New 
import Camphor.Base.Base_Step2.Auxilary
import Camphor.Base.Base_Step2.Base_Step2_2(parser2_2)
import Camphor.Base.Base_Step2.PCS_Parser(parser2')
import Camphor.Base.Base_Step2.Replacer2(replacer3)
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import Camphor.Global.Operators
import Camphor.NonEmpty as NE
import Text.Parsec  
import Text.Parsec.Pos(newPos)
import qualified Data.Map as M
 
step2 ::  FilePath -> Txt -> Either ParseError Txt
step2 file txt = do
 xs <- parse parser2'  (file ++ "-step2"  ) txt
 ys <- runParser parser2_2 [] (file ++ "-step2-2") xs
 zs <- convert ys
 return zs
 
defaultStat :: UserState 
defaultStat = emptyState

convert :: Sents -> Either ParseError Txt
convert xs = convert2 defaultStat xs 

convert2 :: UserState -> Sents -> Either ParseError Txt
convert2 stat sents = snd <$> convert2_2 stat sents

convert2_3 :: UserState -> Sents -> Either ParseError (UserState,Txt)
convert2_3 stat = convert2_2 (clearTmp stat)

{------------------------------------------------------------------------------------- 
 -                              * definition of convert2_2 *                           -
 -------------------------------------------------------------------------------------}

convert2_2 :: UserState -> Sents -> Either ParseError (UserState,Txt)
convert2_2 stat []                         = Right (stat,"") 
convert2_2 stat (Single _  (Comm comm):xs) = ("/*" ++ comm ++ "*/") <++$$> convert2_3 stat xs
convert2_2 stat (Single _  (Sp   sp  ):xs) = sp                     <++$$> convert2_2 stat xs -- INTENTIONALLY LEFT AS convert2_2 
convert2_2 stat (Single _  (Scolon   ):xs) = ";"                    <++$$> convert2_3 stat xs
convert2_2 stat (Single _  (Pleq (Var ident) (Constant integer)):xs) = (ident ++ "+=" ++ show integer ++ ";") <++$$> convert2_3 stat xs
convert2_2 stat (Single _  (Mneq (Var ident) (Constant integer)):xs) = (ident ++ "-=" ++ show integer ++ ";") <++$$> convert2_3 stat xs
convert2_2 stat (Single _  (Rd   (Var idnt)):xs) = ("read("  ++ idnt ++ ")" ++ ";") <++$$> convert2_3 stat xs
convert2_2 stat (Single _  (Wrt  (Var idnt)):xs) = ("write(" ++ idnt ++ ")" ++ ";") <++$$> convert2_3 stat xs
convert2_2 _    (Single _  (Pleq _ _) :_) = Left $newErrorMessage(Message$"FIXME:: code 0002")(newPos "__FIXME__" 0 0) 
convert2_2 _    (Single _  (Mneq _ _) :_) = Left $newErrorMessage(Message$"FIXME:: code 0002")(newPos "__FIXME__" 0 0) 
convert2_2 _    (Single _  (Rd   _  ) :_) = Left $newErrorMessage(Message$"FIXME:: code 0002")(newPos "__FIXME__" 0 0) 
convert2_2 _    (Single _  (Wrt  _  ) :_) = Left $newErrorMessage(Message$"FIXME:: code 0002")(newPos "__FIXME__" 0 0) 

convert2_2 stat (Single _  (Pragma prgm):xs) = case prgm of
 ("MEMORY":"using":vars) -> convert2_2 (setTmp vars stat) xs -- INTENTIONALLY LEFT AS convert2_2 
 _                       -> ("/*# " ++ unwords prgm ++ " #*/") <++$$> convert2_3 stat xs

convert2_2 stat (Single pos (Char iden) :xs) = do
 newStat <- newC pos iden stat 
 ("char " ++ iden ++ ";") <++$$> convert2_3 newStat xs
 
convert2_2 stat (Single pos (Del  iden) :xs) = do
 newStat <- newD pos iden stat 
 ("delete " ++ iden ++ ";") <++$$> convert2_3 newStat xs
 
convert2_2 stat (Single pos (Infl fixity op) :xs) = do
 newStat <- newL pos fixity op stat 
 convert2_3 newStat xs 
 
convert2_2 stat (Single pos (Infr fixity op) :xs) = do
 newStat <- newR pos fixity op stat 
 convert2_3 newStat xs 

convert2_2 stat (Single pos (Func1 name typelist sent):xs) = do
 newStat <- newF1 pos name typelist (Just sent) stat
 convert2_3 newStat xs
 
convert2_2 stat (Single pos (Func1Nul name typelist):xs) = do
 newStat <- newF1 pos name typelist Nothing stat 
 convert2_3 newStat xs 
  
convert2_2 stat (Single pos (Func2 op typelist1 typelist2 sent):xs) = do
 newStat <- newF2 pos op typelist1 typelist2 (Just sent) stat
 convert2_3 newStat xs
 
convert2_2 stat (Single pos (Func2Nul op typelist1 typelist2):xs) = do
 newStat <- newF2 pos op typelist1 typelist2 Nothing stat
 convert2_3 newStat xs 
 
convert2_2 stat (Single pos (Call1 name valuelist):xs) = do
 result <- newK1 pos name valuelist stat
 result <++$$> convert2_3 stat xs
 
convert2_2 stat (Block p ys:xs) = do
 (newStat,res) <- convert2_3 (addVFBlock stat) ys
 let result = "{" ++ res ++ "}"
 let remainingVars = getTopVFBlock newStat
 let pos = getLastPos (Block p ys)
 if not$M.null remainingVars then Left$newErrorMessage(Message$"identifiers not deleted")pos else do
 newStat3 <- deleteTopVFBlock newStat $ newErrorMessage(Message$"FIXME: code 0004 ")pos
 result <++$$> convert2_3 newStat3 xs

-- FIXME: does not replace a function call when it's followed by a block
convert2_2 stat (Single _ (Call1WithBlock name valuelist pos2 block):xs) = showCall name valuelist <++$$> convert2_3 stat (Block pos2 block:xs)
 where 
  showCall :: Ident -> ValueList -> String
  showCall nm (SepList(v,ovs)) = nm ++ "(" ++ show' v ++ concat[ (unOp o2) ++ show' v2 | (o2,v2) <- ovs ] ++ ")"
  
--- (val [op val])op(val [op val]);
convert2_2 stat (Single pos (Call2 op valuelist1 valuelist2):xs) = do
 result <- newK2 pos op valuelist1 valuelist2 stat
 result <++$$> convert2_3 stat xs

--- (val [op val])op val [op val] ; 
convert2_2 stat (Single pos (Call3 op valuelist1 valuelist2):xs) = do
 result <- newK3 pos op valuelist1 valuelist2 stat
 result <++$$> convert2_3 stat xs

--- [val op] (val [op val]) ; 
convert2_2 stat (Single pos (Call4 list valuelist):xs) = do
 result <- newK4 pos list valuelist stat
 result <++$$> convert2_3 stat xs

--- val [op val] op val [op val] ;
convert2_2 stat (Single pos (Call5 valuelist):xs) = do
 result <- newK5 pos valuelist stat
 result <++$$> convert2_3 stat xs
{-----------------------------------------------------------
 -                   * end of convert2_2 *                 -
 -----------------------------------------------------------}
 


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
 result          <- replaceOpMacro pos op valuelist1 valuelist2 stat
 return result

--- no-parenthesized operator call  
newK5 :: SourcePos -> ValueList -> UserState -> Either ParseError Txt
newK5 _   (SepList(Constant _,[])) _     = return "" -- 123; is a nullary sentence
newK5 pos (SepList(Var ident ,[])) stat  = case getVFContents stat ident of
 Nothing        -> Left $newErrorMessage(Message$"identifier "++show ident++" is not defined")pos 
 Just(East ())  -> return ""
 Just(West _ )  -> Left $newErrorMessage(Message$"cannot use variable "++show ident++" because it is already defined as a function")pos  

newK5 pos (SepList(x,ov:ovs)) stat = do
 (oper,vlist1,vlist2) <- getCall5Result pos (x,ov:|ovs) stat
 newK2 pos oper vlist1 vlist2 stat

--- macro-replacing function for operator
replaceOpMacro :: SourcePos -> Oper -> ValueList -> ValueList -> UserState -> Either ParseError Txt
replaceOpMacro pos op valuelist1 valuelist2 stat
 | conflict$ filter isVar $ (toList' valuelist1 ++ toList' valuelist2) = Left$newErrorMessage(Message$"overlapping arguments of operator "++show op)pos 
 | otherwise = do
  instnce <- getInstanceOfCall2 pos op valuelist1 valuelist2 stat
  replacerOfOp (Operator op instnce) instnce valuelist1 valuelist2 stat pos
   
--- macro-replacing function for operator   
replaceFuncMacro :: SourcePos -> Ident -> ValueList -> UserState -> Either ParseError Txt   
replaceFuncMacro pos ident valuelist stat 
 | valuelistIdentConflict valuelist = Left$newErrorMessage(Message$"overlapping arguments of function "++show ident)pos 
 | otherwise = do
  instnce <- getInstanceOfCall1 pos ident valuelist stat
  replacerOfFunc (Func ident instnce) instnce valuelist stat pos 

replacerOfOp :: MacroId -> OpInstance -> ValueList -> ValueList -> UserState -> SourcePos -> Either ParseError Txt
replacerOfOp opname (typelist1,typelist2,sent') valuelist1 valuelist2 stat pos = 
 case sent' of 
  Nothing   -> Left$newErrorMessage(Message$"cannot call operator "++getName opname++" because it is defined as null")pos 
  Just sent -> replacer opname sent stat $ makeReplacerTable2 (typelist1,typelist2) (valuelist1,valuelist2)

replacerOfFunc :: MacroId -> VFInstance -> ValueList -> UserState -> SourcePos -> Either ParseError Txt
replacerOfFunc funcname (typelist,sent') valuelist stat pos =
 case sent' of
  Nothing   -> Left$newErrorMessage(Message$"cannot call function "++getName funcname++" because it is defined as null")pos
  Just sent -> replacer funcname sent stat $ makeReplacerTable typelist valuelist
 
replacer :: MacroId -> Sent -> UserState -> ReplTable -> Either ParseError Txt
replacer mname sent stat table = do
 result <- simplyReplace mname sent stat table
 convert2 stat result -- FIXME:: state not passed
 
simplyReplace :: MacroId -> Sent -> UserState -> ReplTable -> Either ParseError Sents
simplyReplace mname sent stat table = fst <$> simplyReplaceRVC mname sent stat table M.empty
 
-- simplyReplaceRegardingVariableCollision 
simplyReplaceRVC :: MacroId -> Sent -> UserState -> ReplTable -> CollisionTable -> Either ParseError (Sents,CollisionTable)
simplyReplaceRVC mname (Single pos2 ssent) stat table clt = do
 let using = getTmp stat; 
 (newSents,clTable) <- replacer3 (clearTmp stat) (nE mname) pos2 ssent table using clt
 return(map (Single pos2)(toList' newSents),clTable)
simplyReplaceRVC mname (Block p xs) stat table clt = do
 (results,clTable) <- forStatM xs (\ssent tbl -> simplyReplaceRVC mname ssent stat table tbl) clt
 {- forStatM is defined in Camphor.Global.Utilities as `forStatM :: (Monad m) => [a] -> (a -> b -> m (c,b)) -> b -> m ([c],b)' 
 -- and can be used to sequence with passing a state  -}
 return ([Block p $ concat results],clTable) 