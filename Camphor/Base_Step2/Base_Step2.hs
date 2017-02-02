{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
{- Functional macro expansion -}
module Camphor.Base_Step2.Base_Step2
(step2
--,parser2'
) where

import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Data.Ord(comparing)
import Camphor.Partial
import Camphor.Base_Step2.Base_Step2_2
import Camphor.Base_Step2.Auxilary
import Camphor.Base_Step2.PCS_Parser(parser2')
import Camphor.Base_Step2.UserState
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import Camphor.Global.Operators
import Camphor.NonEmpty
import Text.Parsec 
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

{------------------------------------------------------------------------------------- 
 -                              **************************                           -
 -                              * definition of convert2 *                           -
 -                              **************************                           -
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
   showCall nm (v,ovs) = nm ++ "(" ++ show' v ++ concat[ o2 ++ show' v2 | (o2,v2) <- ovs ] ++ ")"
   

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
 -                   *******************                   -
 -                   * end of convert2 *                   -
 -                   *******************                   -
 -----------------------------------------------------------}



newC :: SourcePos -> Ident -> UserState -> Either ParseError UserState
newC pos ident stat 
 | stat `containsIdent` ident = Left $newErrorMessage(Message$"identifier "++show ident++" is already defined")pos
 | otherwise                  = Right$addIdent stat ident (Left())

newD :: SourcePos -> Ident -> UserState -> Either ParseError UserState
newD pos ident stat 
 | stat `containsIdent` ident = Right$removeIdent stat ident -- functions can also be deleted
 | otherwise                  = Left $newErrorMessage(Message$"identifier "++show ident++" is not defined")pos

newL :: SourcePos -> Fix -> Oper -> UserState -> Either ParseError UserState
newL pos fixity op stat = case getOpContents stat op of
 Just(fix,_) -> 
  if fix == InfixL fixity op 
  then Right stat 
  else Left$newErrorMessage(Message$"conflicting fixity definitions of operator "++show op)pos
 Nothing     -> Right$addOpFixity stat (InfixL fixity op)

 
 
newB :: UserState -> Sents -> Either ParseError (UserState,Txt)
newB stat ys = do
 (newStat,res) <-  newB2 stat ys
 return(newStat,"{" ++ res ++ "}")

newB2 :: UserState -> Sents -> Either ParseError (UserState,Txt)
newB2 stat ys = do
 txt <- convert2 stat ys -- FIXME : state is not passed
 return(stat,txt)


newR :: SourcePos -> Fix -> Oper -> UserState -> Either ParseError UserState
newR pos fixity op stat = case getOpContents stat op of
 Just(fix,_) -> 
  if fix == InfixR fixity op 
  then Right stat 
  else Left$newErrorMessage(Message$"conflicting fixity definitions of operator "++show op)pos
 Nothing     -> Right$addOpFixity stat (InfixR fixity op)

-- Function definition
newF1 :: SourcePos -> Ident -> TypeList -> Sent -> UserState -> Either ParseError UserState
newF1 pos name typelist sent stat = case getVFContents stat name of
 Just(Left ())  -> Left $newErrorMessage(Message$"cannot define function"++show name++" because it is already defined as a variable")pos
 Nothing        -> Right$addIdent stat name (Right[(typelist,sent)])
 Just(Right xs) -> Right$addIdent stat name (Right$(typelist,sent):xs)
   

-- Operator definition 
newF2 :: SourcePos -> Oper -> TypeList -> TypeList -> Sent -> UserState -> Either ParseError UserState
newF2 pos op typelist1 typelist2 sent stat@(UserState vflist oplist) = case getOpContents stat op of
 Nothing        -> Left $newErrorMessage(Message$"fixity of operator "++show op++" is not defined")pos
 Just(fix,list) -> Right$UserState vflist newOplist 
  where 
   newOplist = M.insert op (fix,newlist) oplist
   newlist = (typelist1,typelist2,sent):list -- FIXME : does not check the double definition

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
 opfixity <- getOpFixity pos stat op
 ops      <- getOpsFixities pos stat valuelist2
 mapM_ (`canBeRightOf'` opfixity) ops
 result   <- replaceOpMacro pos op valuelist1 valuelist2 stat
 return result -- stat is unchanged
 where 
  canBeRightOf' = canBeRightOf pos

--- Call4 [(Value,Oper)] ValueList
--- right-parenthesized operator call
newK4 :: SourcePos -> [(Value,Oper)] -> ValueList -> UserState -> Either ParseError Txt
newK4 pos [] valuelist stat = newK5 pos valuelist stat -- (val op val); thus is a Call5
newK4 pos (x:xs) valuelist2 stat = do
 opfixity <- getOpFixity pos stat op
 ops      <- getOpsFixities pos stat valuelist1
 mapM_ (`canBeLeftOf'` opfixity) ops
 result   <- replaceOpMacro pos op valuelist1 valuelist2 stat
 return result
 where
  (top,mid,op) = shiftPair (x :| xs)
  valuelist1 :: ValueList
  valuelist1 = (top,mid)
  canBeLeftOf' = canBeLeftOf pos

--- no-parenthesized operator call  
newK5 :: SourcePos -> ValueList -> UserState -> Either ParseError Txt
newK5 _   (Constant _,[]) _     = return "" -- 123; is a nullary sentence
newK5 pos (Var ident ,[]) stat  = case getVFContents stat ident of
   Nothing        -> Left $newErrorMessage(Message$"identifier "++show ident++" is not defined")pos 
   Just(Left ())  -> return ""
   Just(Right _)  -> Left $newErrorMessage(Message$"cannot use variable "++show ident++" because it is already defined as a function")pos
   
newK5 pos (x,ov:ovs) stat = do
 (y:|ys) <- getOpsFixities' pos stat (x,ov:|ovs)
 newK5' pos (x,ov:|ovs) stat (y:|ys)
 

newK5' :: SourcePos -> NonEmptyValue -> UserState -> NonEmpty Fixity -> Either ParseError Txt
newK5' pos nEvaluelist stat fixes = do
 let minOps = minimumsBy (comparing getFixValue) fixes
 case minOps of 
  k                 :| [] -> newK5_2 pos nEvaluelist (getOpName k) stat
  k@(InfixL int op) :| ks -> case contradiction(k:ks) of
   Nothing -> undefined;
   Just k2 -> Left $newErrorMessage(Message$"cannot mix "++show' k++" and "++show' k2++" in the same infix expression")pos -- message borrowed from GHC
  k@(InfixR int op) :| ks -> undefined;
 undefined
 
newK5_2 :: SourcePos -> NonEmptyValue -> Oper -> UserState -> Either ParseError Txt
newK5_2 pos nEvaluelist oper stat = newK2 pos oper vlist1 vlist2 stat
 where (vlist1,vlist2) = breakBy' oper nEvaluelist 


--- macro-replacing function for operator
replaceOpMacro :: SourcePos -> Oper -> ValueList -> ValueList -> UserState -> Either ParseError Txt
replaceOpMacro pos op valuelist1 valuelist2 stat = do
 opinfo <- opinfo'
 let matchingOpInstance = [ a | a@(typelist1,typelist2,_) <- opinfo, valuelist1 `matches` typelist1, valuelist2 `matches` typelist2 ] 
 case matchingOpInstance of 
  []        -> Left $newErrorMessage(Message$"no type-matching instance of "++show op++" defined")pos 
  [instnce] -> replacerOfOp instnce valuelist1 valuelist2 stat
  xs        -> Left $newErrorMessage(Message$show(length xs)++" type-matching instances of "++show op++" defined")pos 
 where
  opinfo' :: Either ParseError [(TypeList,TypeList, Sent)]
  opinfo' = fmap snd $ getOpContents2 pos stat op

--- macro-replacing function for operator   
replaceFuncMacro :: SourcePos -> Ident -> ValueList -> UserState -> Either ParseError Txt   
replaceFuncMacro pos ident valuelist stat = do
 finfo <- finfo'
 let matchingFuncInstance = [ a | a@(typelist,_) <- finfo, valuelist `matches` typelist ]
 case matchingFuncInstance of
  []        -> Left $newErrorMessage(Message$"no type-matching instance of "++show ident++" defined")pos  
  [instnce] -> replacerOfFunc instnce valuelist stat
  xs        -> Left $newErrorMessage(Message$show(length xs)++" type-matching instances of "++show ident++" defined")pos   
 where
  finfo' :: Either ParseError [(TypeList, Sent)]
  finfo' = case getVFContents stat ident of 
   Nothing          -> Left $newErrorMessage(Message$"function "++show ident++" is not defined")pos 
   Just(Left())     -> Left $newErrorMessage(Message$"cannot call"++show ident++" because it is defined as a variable")pos
   Just(Right info) -> Right $ info

replacerOfOp :: (TypeList,TypeList, Sent) -> ValueList -> ValueList -> UserState -> Either ParseError Txt
replacerOfOp (typelist1,typelist2,sent) valuelist1 valuelist2 stat = 
 replacer sent stat $ makeReplacerTable2 (typelist1,typelist2) (valuelist1,valuelist2)

replacerOfFunc :: (TypeList, Sent) -> ValueList -> UserState -> Either ParseError Txt
replacerOfFunc (typelist,sent) valuelist stat =
 replacer sent stat $ makeReplacerTable typelist valuelist

replacer :: Sent -> UserState -> ReplTable -> Either ParseError Txt
replacer (Single(pos2,ssent)) stat table = do
 newSSent <- replacer2 pos2 ssent table
 convert2 stat [Single(pos2,newSSent)]
replacer (Block xs) stat table = do
 result <- sequence [replacer ssent stat table | ssent <- xs]
 return$concat(["{"]++result++["}"])


{-  -------------------------------------------------------------------------------
   ***************************
   * definition of replacer2 *
   ***************************
----------------------------------------------------------------------------------}   
replacer2 :: SourcePos -> SimpleSent -> M.Map Ident Value ->  Either ParseError SimpleSent

{- 
data SimpleSent =
 Func1 Ident TypeList Sent | Func2 Oper TypeList TypeList Sent | Call1 Ident ValueList |
 Call2 Oper ValueList ValueList | Call3 Oper ValueList ValueList | Call4 [(Value,Oper)] ValueList | Call5 ValueList deriving(Show)
-} 

replacer2 _ Scolon   _ = return Scolon
replacer2 _ (Sp x)   _ = return (Sp x)
replacer2 _ (Comm x) _ = return (Comm x)
replacer2 pos (Infl _ _) _ = Left$newErrorMessage(Message$"cannot decleare fixity inside function/operator definition ")pos  
replacer2 pos (Infr _ _) _ = Left$newErrorMessage(Message$"cannot decleare fixity inside function/operator definition ")pos  

replacer2 pos (Char ident) table = case M.lookup ident table of
 Nothing -> return(Char ident)
 Just _  -> Left$newErrorMessage(Message$"cannot redefine an argument "++show ident)pos 
 
replacer2 pos (Del ident) table = case M.lookup ident table of
 Nothing -> return(Del ident)
 Just _  -> Left$newErrorMessage(Message$"cannot delete an argument"++show ident)pos 
 
{-  -------------------------------------------------------------------------------
   ********************
   * end of replacer2 *
   ********************
----------------------------------------------------------------------------------}