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
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader

 
step2 ::  FilePath -> Txt -> Either ParseError Txt
step2 file txt = do
 xs <- parse parser2'  (file ++ "-step2") txt
 ys <- runParser parser2_2 [] (file ++ "-step2-2") xs
 zs <- convert ys
 return zs
 
defaultStat :: UserState 
defaultStat = emptyState

convert :: Sents -> Either ParseError Txt
convert xs = convert2 defaultStat xs 

convert2 :: UserState -> Sents -> Either ParseError Txt
convert2 stat sents = evalStateT (convert2_2 sents) stat 

convert2_3 :: Sents -> StateT UserState (Either ParseError) Txt
convert2_3 sents = do
 modify clearTmp
 convert2_2 sents

infixl 4 <++?>
(<++?>) :: (Monad m,Monoid a) => a -> StateT s m a -> StateT s m a
a <++?> f = do
 b <- f 
 return(a `mappend` b)

complex :: (Monoid a,Monad m) => StateT s m a -> (s -> m s) -> a -> StateT s m a
complex g f result = do
 StateT $ (\s -> do{x <- f s; return((),x)})
 result <++?> g
 
complex2 :: (Monoid a,Monad m) => StateT s m a -> ReaderT s m a -> StateT s m a
complex2 g f = do
 result <- toState f 
 result <++?> g

complex3 :: (Monoid a,Monad m) => StateT s m a -> StateT s m a -> StateT s m a
complex3 g f = do
 result <- f 
 result <++?> g

{------------------------------------------------------------------------------------- 
 -                              * definition of convert2_2 *                           -
 -------------------------------------------------------------------------------------}

convert2_2 :: Sents -> StateT UserState (Either ParseError) Txt
convert2_2 []                                    = return "" 
convert2_2 (Single _    (Comm comm):xs)          = ("/*" ++ comm ++ "*/")                         <++?> convert2_3 xs 
convert2_2 (Single _    (Sp   sp  ):xs)          = sp                                             <++?> convert2_2 xs  -- convert2_2 INTENTIONAL
convert2_2 (Single _    (Scolon   ):xs)          = ""                                             <++?> convert2_3 xs 
convert2_2 (Single _    (Pleq ident integer):xs) = (unId ident ++ "+=" ++ showNum integer ++ ";") <++?> convert2_3 xs 
convert2_2 (Single _    (Mneq ident integer):xs) = (unId ident ++ "-=" ++ showNum integer ++ ";") <++?> convert2_3 xs 
convert2_2 (Single _    (Rd   idnt):xs)          = ("read("  ++ unId idnt ++ ")" ++ ";")          <++?> convert2_3 xs 
convert2_2 (Single _    (Wrt  idnt):xs)          = ("write(" ++ unId idnt ++ ")" ++ ";")          <++?> convert2_3 xs 

convert2_2 (Single pos  (Pragma prgm):xs) = case prgm of
 ("MEMORY":"using":vars) -> do
  let vars' = forM vars toIdent2
  case vars' of 
   Left e -> err$newErrorMessage(Message$showStr e++" used in a `MEMORY using' pragma is not an identifier")pos 
   Right vars_ -> complex (convert2_2 xs) (return . setTmp vars_) "" -- INTENTIONALLY LEFT AS convert2_2 
 _                       -> ("/*# " ++ unwords prgm ++ " #*/") <++?> convert2_3 xs

convert2_2 (Single pos (Char iden)                  :xs) = complex (convert2_3 xs) (newC pos iden)                         ("char "   ++ unId iden ++ ";")
convert2_2 (Single pos (Del  iden)                  :xs) = complex (convert2_3 xs) (newD pos iden)                         ("delete " ++ unId iden ++ ";")
convert2_2 (Single pos (Infl fixity op)             :xs) = complex (convert2_3 xs) (newL pos fixity op)                     ""
convert2_2 (Single pos (Infr fixity op)             :xs) = complex (convert2_3 xs) (newR pos fixity op)                     ""
convert2_2 (Single pos (Func1 name typelist sent)   :xs) = complex (convert2_3 xs) (newF1 pos name typelist (Just sent))    ""
convert2_2 (Single pos (Func1Nul name typelist)     :xs) = complex (convert2_3 xs) (newF1 pos name typelist Nothing    )    ""
convert2_2 (Single pos (Func2 op tlist1 tlist2 sent):xs) = complex (convert2_3 xs) (newF2 pos op tlist1 tlist2 (Just sent)) ""
convert2_2 (Single pos (Func2Nul op tlist1 tlist2)  :xs) = complex (convert2_3 xs) (newF2 pos op tlist1 tlist2 Nothing    ) ""
convert2_2 (Single pos (Syntax1 name tl arg bl)     :xs) = complex (convert2_3 xs) (newS1 pos name tl arg bl)               ""
convert2_2 (Single pos (Syntax2 name ttl arg bl)    :xs) = complex (convert2_3 xs) (newS2 pos name ttl arg bl)              ""
 
convert2_2 (Single pos (Call1 name valuelist)          :xs) = complex2 (convert2_3 xs) (newK1 pos name valuelist) 
convert2_2 (Single pos (Call2 op valuelist1 valuelist2):xs) = complex2 (convert2_3 xs) (newK2 pos op valuelist1 valuelist2) --- (val [op val])op(val [op val]);
convert2_2 (Single pos (Call3 op valuelist1 valuelist2):xs) = complex2 (convert2_3 xs) (newK3 pos op valuelist1 valuelist2) --- (val [op val])op val [op val] ; 
convert2_2 (Single pos (Call4 list valuelist)          :xs) = complex2 (convert2_3 xs) (newK4 pos list valuelist) --- [val op] (val [op val]) ; 
convert2_2 (Single pos (Call5 valuelist)               :xs) = complex2 (convert2_3 xs) (newK5 pos valuelist) --- val [op val] op val [op val] ;
convert2_2 (Single _ (Call1WithBlock name valuelist pos2 block):xs) = showCall name valuelist <++?> convert2_3 (Block pos2 block:xs)
 where 
  showCall :: Ident2 -> ValueList -> String
  showCall nm (SepList v ovs) = unId nm ++ "(" ++ show' v ++ concat[ (unOp o2) ++ show' v2 | (o2,v2) <- ovs ] ++ ")" 
 
convert2_2 (Block p ys:xs) = complex3 (convert2_3 xs) (newStat3getter p ys)
{-----------------------------------------------------------
 -                   * end of convert2_2 *                 -
 -----------------------------------------------------------}
newStat3getter :: SourcePos -> Sents -> StateT UserState (Either ParseError) Txt
newStat3getter p ys = do
 let pos = getLastPos (Block p ys)
 modify addVFBlock
 res <- convert2_3 ys
 let result = "{" ++ res ++ "}"
 newStat <- get
 let remainingVars = getTopVFBlock newStat
 if not $ M.null remainingVars then err$newErrorMessage(Message$"identifiers not deleted")pos else return ()
 deleteTopVFBlock_ (newErrorMessage(Message$"FIXME: code 0004 ")pos)
 return result
 
newS1 :: SourcePos -> Ident2 -> TypeList -> Ident2 -> Sent -> UserState -> Either ParseError UserState 
newS1 _ _ _ _ _ stat = Right stat -- fixme 

newS2 :: SourcePos -> Ident2 -> TailTypeList -> Ident2 -> Sent -> UserState -> Either ParseError UserState 
newS2 _ _ _ _ _ stat = Right stat -- fixme 
 
-- Function call
newK1 :: SourcePos -> Ident2 -> ValueList -> ReaderT UserState (Either ParseError) Txt
newK1 pos name valuelist = do
 result <- replaceFuncMacro pos name valuelist 
 return result -- stat is unchanged
   
-- normalized operator call
newK2 :: SourcePos -> Oper -> ValueList -> ValueList -> ReaderT UserState (Either ParseError) Txt
newK2 pos op valuelist1 valuelist2 = do
 result <- replaceOpMacro pos op valuelist1 valuelist2
 return result -- stat is unchanged
 
-- left-parenthesized operator call
newK3 :: SourcePos -> Oper -> ValueList -> ValueList -> ReaderT UserState (Either ParseError) Txt
newK3 pos op valuelist1 valuelist2 = do
 stat <- ask
 lift $ isValidCall3 pos op valuelist2 stat
 result <- replaceOpMacro pos op valuelist1 valuelist2
 return result -- stat is unchanged

--- Call4 [(Value,Oper)] ValueList
--- right-parenthesized operator call
newK4 :: SourcePos -> [(Value,Oper)] -> ValueList -> ReaderT UserState (Either ParseError) Txt
newK4 pos [] valuelist = newK5 pos valuelist -- (val op val); thus is a Call5
newK4 pos (x:xs) valuelist2 = do
 stat <- ask
 (valuelist1,op) <- lift $ getCall4Left pos (x:|xs) stat
 result          <- replaceOpMacro pos op valuelist1 valuelist2 
 return result

--- no-parenthesized operator call  
newK5 :: SourcePos -> ValueList -> ReaderT UserState (Either ParseError) Txt
newK5 _   (SepList(Constant _)[])      = return "" -- 123; is a nullary sentence
newK5 pos (SepList(Var ident )[])      = do
 stat <- ask
 case getVFContents stat ident of
  Nothing        -> err$newErrorMessage(Message$"identifier "++showIdent ident++" is not defined")pos 
  Just(East ())  -> return ""
  Just(West _ )  -> err$newErrorMessage(Message$"cannot use variable "++showIdent ident++" because it is already defined as a function")pos  

newK5 pos (SepList x(ov:ovs)) = do
 stat <- ask
 (oper,vlist1,vlist2) <- lift $ getCall5Result pos (x,ov:|ovs) stat
 result <- newK2 pos oper vlist1 vlist2
 return result

--- macro-replacing function for operator
replaceOpMacro :: SourcePos -> Oper -> ValueList -> ValueList -> ReaderT UserState (Either ParseError) Txt
replaceOpMacro pos op valuelist1 valuelist2 
 | conflict $ filter isVar $ (toList' valuelist1 ++ toList' valuelist2) = err$newErrorMessage(Message$"overlapping arguments of operator "++showStr(unOp op))pos 
 | otherwise = do
  stat <- ask
  instnce <- lift $ getInstanceOfCall2 pos op valuelist1 valuelist2 stat
  result <- replacerOfOp (Operator op instnce) instnce valuelist1 valuelist2 pos 
  return result
   
--- macro-replacing function for operator   
replaceFuncMacro :: SourcePos -> Ident2 -> ValueList -> ReaderT UserState (Either ParseError) Txt   
replaceFuncMacro pos ident valuelist  
 | valuelistIdentConflict valuelist = err$newErrorMessage(Message$"overlapping arguments of function "++showIdent ident)pos 
 | otherwise = do
  stat <- ask
  instnce <- lift $ getInstanceOfCall1 pos ident valuelist stat
  replacerOfFunc (Func ident instnce) instnce valuelist pos		

replacerOfOp :: MacroId -> OpInstance -> ValueList -> ValueList -> SourcePos -> ReaderT UserState (Either ParseError) Txt
replacerOfOp opname (typelist1,typelist2,sent') valuelist1 valuelist2 pos =   
 case sent' of 
  Nothing   -> err$newErrorMessage(Message$"cannot call operator "++getName opname++" because it is defined as null")pos 
  Just sent -> replacer opname sent (makeReplacerTable2 (typelist1,typelist2) (valuelist1,valuelist2)) 

replacerOfFunc :: MacroId -> VFInstance -> ValueList -> SourcePos -> ReaderT UserState (Either ParseError) Txt
replacerOfFunc funcname (typelist,sent') valuelist pos =
 case sent' of
  Nothing   -> err$newErrorMessage(Message$"cannot call function "++getName funcname++" because it is defined as null")pos
  Just sent -> replacer funcname (toSent2 sent) (makeReplacerTable typelist valuelist) 
 
replacer :: MacroId -> Sent2 -> ReplTable -> ReaderT UserState (Either ParseError) Txt
replacer mname sent table = do
 result <- simplyReplace mname sent table
 stat <- ask
 lift $ convert2 stat result -- FIXME:: state of convert2 not passed
 
simplyReplace :: MacroId -> Sent2 -> ReplTable -> ReaderT UserState (Either ParseError) Sents
simplyReplace mname sent table = ReaderT $ \stat -> evalStateT (simplyReplaceRVC mname sent stat table) (M.empty,getTmp stat,())
 
-- simplyReplaceRegardingVariableCollision 
simplyReplaceRVC :: MacroId -> Sent2 -> UserState -> ReplTable -> StateT (CollisionTable,Maybe TmpStat,()) (Either ParseError) Sents
simplyReplaceRVC mname (Single pos2 ssent) stat table = do
 newSents <- replacer3 (clearTmp stat) (nE mname) pos2 ssent table
 return newSents
 
simplyReplaceRVC mname (Block p xs) stat table = do
 results <- forM xs (\ssent -> simplyReplaceRVC mname ssent stat table) 
 return [Block p $ concat results]