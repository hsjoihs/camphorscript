{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base.Base_Step2.Call
(Cnv2,Cnv23
,callK1,callK2,callK3,callK4,callK5
,callSC1,callSC2
,newStat3getter
,replacer,syntaxer
) where
import Camphor.SafePrelude
import Camphor.TupleTrans
import Camphor.SepList as Sep
import Camphor.Base.Base_Step2.Type
import Camphor.Base.Base_Step2.UserState
import Camphor.Base.Base_Step2.Auxilary2 
import Camphor.Base.Base_Step2.Call5Result
import Camphor.Base.Base_Step2.Auxilary
import Camphor.Base.Base_Step2.ErrList
import Camphor.Base.Base_Step2.Replacer2(replacer3)
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import Camphor.NonEmpty as NE
import Text.Parsec
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
type Cnv2 = UserState -> Sents -> Either ParseError Txt
type Cnv23 = Sents -> StateT UserState (Either ParseError) Txt

--- syntax call 1
callSC1 :: SourcePos -> Ident2 -> ValueList -> (SourcePos,Sents) -> Cnv23 -> ReaderT (UserState,Cnv2) (Either ParseError) String
callSC1 _   name (SepList (Var i) _) (pos2,block) cnv23
 | unId name == "while" = do
  res <- newStat3getter pos2 block cnv23
  return $ "while(" ++ unId i ++ ")" ++ res
callSC1 pos name vl (pos2,block) _
 | valuelistIdentConflict vl = err$toPE pos $ Step2 <!> Type <!> WrongCall <!> Argoverlap <!> Synt name 
 | otherwise = do
  stat <- askFst
  (instnce,repl) <- lift $ getInstanceOfSC pos name (West vl) stat
  syntaxer name instnce repl (pos2,block)
 
newStat3getter :: SourcePos -> Sents -> Cnv23 -> ReaderT (UserState,Cnv2) (Either ParseError) Txt
newStat3getter p ys cnv23 = fromState $ do
 let pos = getLastPos (Block p ys)
 origStat <- getFst
 c2 <- getSnd
 modifyFst addVFBlock
 res <- changeState (\x->(x,c2)) fst (cnv23 ys)
 newStat <- getFst
 let remainingVars = map fst $ M.toList $ getTopVFBlock newStat
 case remainingVars of
  [] -> return ()
  (x:xs)  -> err$toPE pos$ Step2 <!> Finish <!> NotDel <!> Idns (x:|xs)
 putFst origStat 
 return $ "{" ++ res ++ "}"

-- Function call
callK1 :: SourcePos -> Ident2 -> ValueList -> ReaderT (UserState,Cnv2) (Either ParseError) Txt
callK1 pos ident valuelist  
 | valuelistIdentConflict valuelist = err$toPE pos $ Step2 <!> Type <!> WrongCall <!> Argoverlap <!> Functi ident
 | otherwise = do
  stat <- askFst
  instnce <- lift $ getInstanceOfCall1 pos ident valuelist stat
  let funcname = Func ident instnce; (typelist,sent') = instnce
  case sent' of
   Nothing   -> err$toPE pos$Step2 <!> Type <!> WrongCall <!> Nulldefined <!> Functi_4 ident
   Just sent -> replacer funcname (toSent2 sent) (makeReplacerTable typelist valuelist) 
   
-- normalized operator call
callK2 :: SourcePos -> Oper -> ValueList -> ValueList -> ReaderT (UserState,Cnv2) (Either ParseError) Txt
callK2 pos op valuelist1 valuelist2
 | conflict $ filter isVar $ (toList' valuelist1 ++ toList' valuelist2) = err$toPE pos $ Step2 <!> Type <!> WrongCall <!> Argoverlap <!> Operat op
 | otherwise = do
  stat <- askFst
  instnce <- lift $ getInstanceOfCall2 pos op valuelist1 valuelist2 stat
  let opname = Operator op instnce; (typelist1,typelist2,sent') = instnce
  case sent' of 
   Nothing   -> err$toPE pos$Step2 <!> Type <!> WrongCall <!> Nulldefined <!> Operat_4 op
   Just sent -> replacer opname sent (makeReplacerTable2 (typelist1,typelist2) (valuelist1,valuelist2))
 
-- left-parenthesized operator call
callK3 :: SourcePos -> Oper -> ValueList -> ValueList -> ReaderT (UserState,Cnv2) (Either ParseError) Txt
callK3 pos op valuelist1 valuelist2 = do
 stat <- askFst
 lift $ isValidCall3 pos op valuelist2 stat
 callK2 pos op valuelist1 valuelist2

--- Call4 [(Value,Oper)] ValueList
--- right-parenthesized operator call
callK4 :: SourcePos -> [(Value,Oper)] -> ValueList -> ReaderT (UserState,Cnv2) (Either ParseError) Txt
callK4 pos [] valuelist = callK5 pos valuelist -- (val op val); thus is a Call5
callK4 pos (x:xs) valuelist2 = do
 stat <- askFst
 (valuelist1,op) <- lift $ getCall4Left pos (x:|xs) stat
 callK2 pos op valuelist1 valuelist2

--- no-parenthesized operator call  
callK5 :: SourcePos -> ValueList -> ReaderT (UserState,Cnv2) (Either ParseError) Txt
callK5 _   (SepList(Constant _)[]) = return "" -- 123; is a nullary sentence
callK5 pos (SepList(Var ident )[]) = do
 stat <- askFst
 case getVFContents stat ident of
  Nothing           -> err$toPE pos$ Step2 <!> Access <!> WrongRef <!> Notdefined_3 <!> Idn ident
  Just Variable     -> return ""
  Just (FunSyn _ _) -> err$toPE pos$ Step2 <!> Access <!> WrongRef <!> Definedasfunsyn <!> Idn ident

callK5 pos (SepList x(ov:ovs)) = do
 stat <- askFst
 (oper,vlist1,vlist2) <- lift $ getCall5Result pos (x,ov:|ovs) stat
 callK2 pos oper vlist1 vlist2 



--- syntax call 2  
callSC2 :: SourcePos -> Ident2 -> TailValueList -> (SourcePos,Sents) -> ReaderT (UserState,Cnv2) (Either ParseError) String
callSC2 pos name tvl (pos2,block)
 | valuelistIdentConflict tvl = err$toPE pos $ Step2 <!> Type <!> WrongCall <!> Argoverlap <!> Synt name
 | otherwise = do
  stat <- askFst
  (instnce,repl) <- lift $ getInstanceOfSC pos name (East tvl) stat
  syntaxer name instnce repl (pos2,block) 

  
syntaxer :: Ident2 -> SyntaxInstance -> ReplTable -> (SourcePos,Sents) -> ReaderT (UserState,Cnv2) (Either ParseError) String
syntaxer name instnce@(_, block1) table (_,block2) = do
 let mname = Syn name instnce
 result <- simplyReplace mname (toSent2 block1) table (Just ())
 block3 <- blockInsert result block2
 stat <- askFst
 cnv2 <- askSnd
 lift $ cnv2 stat block3 -- fixme :: state of convert2 not passed
 
 
replacer :: MacroId -> Sent2 -> ReplTable -> ReaderT (UserState,Cnv2) (Either ParseError) Txt
replacer mname sent table = do
 result <- simplyReplace mname sent table Nothing
 stat <- askFst
 cnv2 <- askSnd
 lift $ cnv2 stat result -- fixme :: state of convert2 not passed


blockInsert :: Sents -> Sents -> ReaderT s (Either ParseError) Sents
blockInsert host guest = return $ concatMap (blockInsert' guest) host

blockInsert' :: Sents -> Sent -> Sents
blockInsert' guest (Single _ SynBlock) = guest
blockInsert' guest (Single p (SynCall1 i vl  p2 sents)) = [Single p (SynCall1 i vl  p2$concat[ blockInsert' guest sent | sent <- sents])]
blockInsert' guest (Single p (SynCall2 i tvl p2 sents)) = [Single p (SynCall2 i tvl p2$concat[ blockInsert' guest sent | sent <- sents])]
blockInsert' _ a@(Single _ _) = [a]
blockInsert' guest (Block p sents) = [Block p $ concat[ blockInsert' guest sent | sent <- sents] ]


simplyReplace :: MacroId -> Sent2 -> ReplTable -> Maybe () -> ReaderT (UserState,Cnv2) (Either ParseError) Sents
simplyReplace mname sent table mi = ReaderT $ \(stat,_) -> evalStateT (simplyReplaceRVC mname sent stat table) (M.empty,getTmp stat,mi)
 
-- simplyReplaceRegardingVariableCollision 
simplyReplaceRVC :: MacroId -> Sent2 -> UserState -> ReplTable -> StateT (CollisionTable,Maybe TmpStat,Maybe ()) (Either ParseError) Sents
simplyReplaceRVC mname (Single pos2 ssent) stat table = do
 newSents <- replacer3 (clearTmp stat) (nE mname) pos2 ssent table
 return newSents
 
simplyReplaceRVC mname (Block p xs) stat table = do
 results <- forM xs (\ssent -> simplyReplaceRVC mname ssent stat table) 
 return [Block p $ concat results]