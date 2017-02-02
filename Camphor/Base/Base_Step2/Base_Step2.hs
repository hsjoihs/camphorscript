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
import Camphor.Base.Base_Step2.Call5Result
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
convert2_2 (Single _    (SynBlock ):xs)          = ("/*block*/")                                  <++?> convert2_3 xs 
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
   Right vars_ -> 
    complex (convert2_2 xs) (return . setTmp vars_) (concatMap (\iden -> "assert_zero " ++ unId iden ++ ";") vars_) -- INTENTIONALLY LEFT AS convert2_2 
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
convert2_2 (Single _   (SynCall1 name vl pos2 block)   :xs) = complex2 (convert2_3 xs) (newSC1 pos2 name vl (pos2,block)) 
convert2_2 (Single _   (SynCall2 name tvl pos2 block)  :xs) = complex2 (convert2_3 xs) (newSC2 pos2 name tvl (pos2,block))

 
convert2_2 (Block p ys:xs) = complex3 (convert2_3 xs) (newStat3getter p ys)
{-----------------------------------------------------------
 -                   * end of convert2_2 *                 -
 -----------------------------------------------------------}


 

 
-- Function call
newK1 :: SourcePos -> Ident2 -> ValueList -> ReaderT UserState (Either ParseError) Txt
newK1 pos ident valuelist   
 | valuelistIdentConflict valuelist = err$newErrorMessage(Message$"overlapping arguments of function "++showIdent ident)pos 
 | otherwise = do
  stat <- ask
  instnce <- lift $ getInstanceOfCall1 pos ident valuelist stat
  let funcname = Func ident instnce; (typelist,sent') = instnce
  case sent' of
   Nothing   -> err$newErrorMessage(Message$"cannot call function "++getName funcname++" because it is defined as null")pos
   Just sent -> replacer funcname (toSent2 sent) (makeReplacerTable typelist valuelist)   
   
-- normalized operator call
newK2 :: SourcePos -> Oper -> ValueList -> ValueList -> ReaderT UserState (Either ParseError) Txt
newK2 pos op valuelist1 valuelist2
 | conflict $ filter isVar $ (toList' valuelist1 ++ toList' valuelist2) = err$newErrorMessage(Message$"overlapping arguments of operator "++showStr(unOp op))pos 
 | otherwise = do
  stat <- ask
  instnce <- lift $ getInstanceOfCall2 pos op valuelist1 valuelist2 stat
  let opname = Operator op instnce; (typelist1,typelist2,sent') = instnce
  case sent' of 
   Nothing   -> err$newErrorMessage(Message$"cannot call operator "++getName opname++" because it is defined as null")pos 
   Just sent -> replacer opname sent (makeReplacerTable2 (typelist1,typelist2) (valuelist1,valuelist2)) 
 
-- left-parenthesized operator call
newK3 :: SourcePos -> Oper -> ValueList -> ValueList -> ReaderT UserState (Either ParseError) Txt
newK3 pos op valuelist1 valuelist2 = do
 stat <- ask
 lift $ isValidCall3 pos op valuelist2 stat
 newK2 pos op valuelist1 valuelist2

--- Call4 [(Value,Oper)] ValueList
--- right-parenthesized operator call
newK4 :: SourcePos -> [(Value,Oper)] -> ValueList -> ReaderT UserState (Either ParseError) Txt
newK4 pos [] valuelist = newK5 pos valuelist -- (val op val); thus is a Call5
newK4 pos (x:xs) valuelist2 = do
 stat <- ask
 (valuelist1,op) <- lift $ getCall4Left pos (x:|xs) stat
 newK2 pos op valuelist1 valuelist2 

--- no-parenthesized operator call  
newK5 :: SourcePos -> ValueList -> ReaderT UserState (Either ParseError) Txt
newK5 _   (SepList(Constant _)[])      = return "" -- 123; is a nullary sentence
newK5 pos (SepList(Var ident )[])      = do
 stat <- ask
 case getVFContents stat ident of
  Nothing           -> err$newErrorMessage(Message$"identifier "++showIdent ident++" is not defined")pos 
  Just Variable     -> return "/*aaa*/"
  Just (FunSyn _ _) -> err$newErrorMessage(Message$"cannot use variable "++showIdent ident++" because it is already defined as a function")pos  

newK5 pos (SepList x(ov:ovs)) = do
 stat <- ask
 (oper,vlist1,vlist2) <- lift $ getCall5Result pos (x,ov:|ovs) stat
 newK2 pos oper vlist1 vlist2
{-
-- Function call
newK1 :: SourcePos -> Ident2 -> ValueList -> ReaderT UserState (Either ParseError) Txt
newK1 pos ident valuelist   
 | valuelistIdentConflict valuelist = err$newErrorMessage(Message$"overlapping arguments of function "++showIdent ident)pos 
 | otherwise = do
  stat <- ask
  instnce <- lift $ getInstanceOfCall1 pos ident valuelist stat
  let funcname = Func ident instnce; (typelist,sent') = instnce
  case sent' of
   Nothing   -> err$newErrorMessage(Message$"cannot call function "++getName funcname++" because it is defined as null")pos
   Just sent -> replacer funcname (toSent2 sent) (makeReplacerTable typelist valuelist)  
-}

 
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


--- syntax call 1
newSC1 :: SourcePos -> Ident2 -> ValueList -> (SourcePos,Sents) -> ReaderT UserState (Either ParseError) String
newSC1 _   name (SepList (Var i) _) (pos2,block)
 | unId name == "while" = do
  res <- fromState $ newStat3getter pos2 block 
  return $ "while(" ++ unId i ++ ")" ++ res
newSC1 pos name vl (pos2,block)
 | valuelistIdentConflict vl = err$newErrorMessage(Message$"overlapping arguments of syntax "++showIdent name)pos  
 | otherwise = do
  stat <- ask
  (instnce,repl) <- lift $ getInstanceOfSC pos name (West vl) stat
  syntaxer name instnce repl (pos2,block) 

--- syntax call 2  
newSC2 :: SourcePos -> Ident2 -> TailValueList -> (SourcePos,Sents) -> ReaderT UserState (Either ParseError) String
newSC2 pos name tvl (pos2,block)
 | tailValuelistIdentConflict tvl = err$newErrorMessage(Message$"overlapping arguments of syntax "++showIdent name)pos 
 | otherwise = do
  stat <- ask
  (instnce,repl) <- lift $ getInstanceOfSC pos name (East tvl) stat
  syntaxer name instnce repl (pos2,block)  

  
syntaxer :: Ident2 -> SyntaxInstance -> ReplTable -> (SourcePos,Sents) -> ReaderT UserState (Either ParseError) String
syntaxer name instnce@(_, arg, block1) table (_,block2) = do
 let mname = Syn name instnce
 result <- simplyReplace mname (toSent2 block1) table (Just arg)
 block3 <- blockInsert result block2
 stat <- ask
 lift $ convert2 stat block3 -- fixme :: state of convert2 not passed
 
blockInsert :: Sents -> Sents -> ReaderT UserState (Either ParseError) Sents
blockInsert host guest = return $ concatMap (blockInsert' guest) host

blockInsert' :: Sents -> Sent -> Sents
blockInsert' guest (Single _ SynBlock) = guest
blockInsert' guest (Single p (SynCall1 i vl  p2 sents)) = [Single p (SynCall1 i vl  p2$concat[ blockInsert' guest sent | sent <- sents])]
blockInsert' guest (Single p (SynCall2 i tvl p2 sents)) = [Single p (SynCall2 i tvl p2$concat[ blockInsert' guest sent | sent <- sents])]
blockInsert' _ a@(Single _ _) = [a]
blockInsert' guest (Block p sents) = [Block p $ concat[ blockInsert' guest sent | sent <- sents] ]

 
replacer :: MacroId -> Sent2 -> ReplTable -> ReaderT UserState (Either ParseError) Txt
replacer mname sent table = do
 result <- simplyReplace mname sent table Nothing
 stat <- ask
 lift $ convert2 stat result -- fixme :: state of convert2 not passed
 
simplyReplace :: MacroId -> Sent2 -> ReplTable -> Maybe Ident2 -> ReaderT UserState (Either ParseError) Sents
simplyReplace mname sent table mi = ReaderT $ \stat -> evalStateT (simplyReplaceRVC mname sent stat table) (M.empty,getTmp stat,mi)
 
-- simplyReplaceRegardingVariableCollision 
simplyReplaceRVC :: MacroId -> Sent2 -> UserState -> ReplTable -> StateT (CollisionTable,Maybe TmpStat,Maybe Ident2) (Either ParseError) Sents
simplyReplaceRVC mname (Single pos2 ssent) stat table = do
 newSents <- replacer3 (clearTmp stat) (nE mname) pos2 ssent table
 return newSents
 
simplyReplaceRVC mname (Block p xs) stat table = do
 results <- forM xs (\ssent -> simplyReplaceRVC mname ssent stat table) 
 return [Block p $ concat results]