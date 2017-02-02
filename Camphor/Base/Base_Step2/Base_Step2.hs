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
import Camphor.NonEmpty as NE
import Text.Parsec  
import Text.Parsec.Pos(newPos)
import qualified Data.Map as M
import Control.Monad.State
import Data.Monoid
 
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
convert2 stat sents = fst <$> runStateT (convert2_2 sents) stat 

convert2_3 :: Sents -> StateT UserState (Either ParseError) Txt
convert2_3 sents = do
 stat <- get
 put (clearTmp stat)
 convert2_2 sents

infixl 4 <++$%>
(<++$%>) :: (Monad m,Monoid a) => a -> (s, StateT s m a) -> m (a,s)
a <++$%> (stat,f) = liftM (\(d,c) -> (a `mappend` d,c)) (runStateT f stat)

infixl 4 <++?>
(<++?>) :: (Monad m,Monoid a) => a -> StateT s m a -> StateT s m a
a <++?> f = StateT $ \stat -> a <++$%> (stat,f)

err :: e -> StateT s (Either e) a
err e = StateT $ \_ -> Left e

complex :: (Monoid a,Monad m) => StateT s m a -> (s -> m s) -> a -> StateT s m a
complex g f a = StateT $ \stat -> do
 newStat <- f stat
 a <++$%> (newStat,g)
 
complex2 :: (Monoid a,Monad m) => StateT s m a -> (s -> m a) -> StateT s m a
complex2 g f = StateT $ \stat -> do
 result <- f stat
 result <++$%> (stat,g)  

complex3 :: (Monoid a,Monad m) => StateT s m a -> (s -> m(a,s)) -> StateT s m a
complex3 g f = StateT $ \stat -> do
 (result,newStat) <- f stat
 result <++$%> (newStat,g)
 

{------------------------------------------------------------------------------------- 
 -                              * definition of convert2_2 *                           -
 -------------------------------------------------------------------------------------}

convert2_2 :: Sents -> StateT UserState (Either ParseError) Txt
convert2_2 []                                                   = StateT $ \stat -> Right ("",stat) 
convert2_2 (Single _  (Comm comm):xs)                           = ("/*" ++ comm ++ "*/")                 <++?> convert2_3 xs 
convert2_2 (Single _  (Sp   sp  ):xs)                           = sp                                     <++?> convert2_2 xs  -- convert2_2 INTENTIONAL
convert2_2 (Single _  (Scolon   ):xs)                           = ";"                                    <++?> convert2_3 xs 
convert2_2 (Single _  (Pleq (Var ident) (Constant integer)):xs) = (ident ++ "+=" ++ show integer ++ ";") <++?> convert2_3 xs 
convert2_2 (Single _  (Mneq (Var ident) (Constant integer)):xs) = (ident ++ "-=" ++ show integer ++ ";") <++?> convert2_3 xs 
convert2_2 (Single _  (Rd   (Var idnt)):xs)                     = ("read("  ++ idnt ++ ")" ++ ";")       <++?> convert2_3 xs 
convert2_2 (Single _  (Wrt  (Var idnt)):xs)                     = ("write(" ++ idnt ++ ")" ++ ";")       <++?> convert2_3 xs 
convert2_2 (Single _  (Pleq _ _) :_)                            = err$newErrorMessage(Message$"FIXME:: code 0002")(newPos "__FIXME__" 0 0) 
convert2_2 (Single _  (Mneq _ _) :_)                            = err$newErrorMessage(Message$"FIXME:: code 0002")(newPos "__FIXME__" 0 0) 
convert2_2 (Single _  (Rd   _  ) :_)                            = err$newErrorMessage(Message$"FIXME:: code 0002")(newPos "__FIXME__" 0 0) 
convert2_2 (Single _  (Wrt  _  ) :_)                            = err$newErrorMessage(Message$"FIXME:: code 0002")(newPos "__FIXME__" 0 0) 

convert2_2 (Single _  (Pragma prgm):xs) = case prgm of
 ("MEMORY":"using":vars) -> complex (convert2_2 xs) (return . setTmp vars) "" -- INTENTIONALLY LEFT AS convert2_2 
 _                       -> ("/*# " ++ unwords prgm ++ " #*/") <++?> convert2_3 xs

convert2_2 (Single pos (Char iden)                        :xs) = complex (convert2_3 xs) (newC pos iden)                               ("char " ++ iden ++ ";")
convert2_2 (Single pos (Del  iden)                        :xs) = complex (convert2_3 xs) (newD pos iden)                               ("delete " ++ iden ++ ";")
convert2_2 (Single pos (Infl fixity op)                   :xs) = complex (convert2_3 xs) (newL pos fixity op)                           ""
convert2_2 (Single pos (Infr fixity op)                   :xs) = complex (convert2_3 xs) (newR pos fixity op)                           ""
convert2_2 (Single pos (Func1 name typelist sent)         :xs) = complex (convert2_3 xs) (newF1 pos name typelist (Just sent))          ""
convert2_2 (Single pos (Func1Nul name typelist)           :xs) = complex (convert2_3 xs) (newF1 pos name typelist Nothing    )          ""
convert2_2 (Single pos (Func2 op typelist1 typelist2 sent):xs) = complex (convert2_3 xs) (newF2 pos op typelist1 typelist2 (Just sent)) ""
convert2_2 (Single pos (Func2Nul op typelist1 typelist2)  :xs) = complex (convert2_3 xs) (newF2 pos op typelist1 typelist2 Nothing    ) ""
 
convert2_2 (Single pos (Call1 name valuelist)          :xs) = complex2 (convert2_3 xs) (newK1 pos name valuelist) 
convert2_2 (Single pos (Call2 op valuelist1 valuelist2):xs) = complex2 (convert2_3 xs) (newK2 pos op valuelist1 valuelist2) --- (val [op val])op(val [op val]);
convert2_2 (Single pos (Call3 op valuelist1 valuelist2):xs) = complex2 (convert2_3 xs) (newK3 pos op valuelist1 valuelist2) --- (val [op val])op val [op val] ; 
convert2_2 (Single pos (Call4 list valuelist)          :xs) = complex2 (convert2_3 xs) (newK4 pos list valuelist) --- [val op] (val [op val]) ; 
convert2_2 (Single pos (Call5 valuelist)               :xs) = complex2 (convert2_3 xs) (newK5 pos valuelist) --- val [op val] op val [op val] ;

-- FIXME: does not replace a function call when it's followed by a block
convert2_2 (Single _ (Call1WithBlock name valuelist pos2 block):xs) = showCall name valuelist <++?> convert2_3 (Block pos2 block:xs)
 where 
  showCall :: Ident -> ValueList -> String
  showCall nm (SepList(v,ovs)) = nm ++ "(" ++ show' v ++ concat[ (unOp o2) ++ show' v2 | (o2,v2) <- ovs ] ++ ")" 
 
convert2_2 (Block p ys:xs) = complex3 (convert2_3 xs) (newStat3getter p ys)

newStat3getter :: SourcePos -> Sents -> UserState -> Either ParseError (Txt, UserState)
newStat3getter p ys stat = do 
 (res,newStat) <- runStateT(convert2_3 ys) (addVFBlock stat) 
 let result = "{" ++ res ++ "}"
 let remainingVars = getTopVFBlock newStat
 let pos = getLastPos (Block p ys)
 if not $ M.null remainingVars then Left $ newErrorMessage(Message$"identifiers not deleted")pos else do
 newStat3 <- deleteTopVFBlock newStat $ newErrorMessage(Message$"FIXME: code 0004 ")pos
 return(result,newStat3)


  

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
 (newSents,clTable) <- runStateT(replacer3 (clearTmp stat) (nE mname) pos2 ssent table using) clt
 return(map (Single pos2)(toList' newSents),clTable)
simplyReplaceRVC mname (Block p xs) stat table clt = do
 (results,clTable) <- forStatM xs (\ssent tbl -> simplyReplaceRVC mname ssent stat table tbl) clt
 {- forStatM is defined in Camphor.Global.Utilities as `forStatM :: (Monad m) => [a] -> (a -> b -> m (c,b)) -> b -> m ([c],b)' 
 -- and can be used to sequence with passing a state  -}
 return ([Block p $ concat results],clTable) 