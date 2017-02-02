{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base.Base_Step2.Replacer2
(replacer3
)where
import Camphor.SafePrelude 
import Camphor.SepList(SepList(..)) 
import Camphor.Base.Base_Step2.Type
import Camphor.Base.Base_Step2.UserState
import Camphor.Base.Base_Step2.New 
import Camphor.Base.Base_Step2.Auxilary
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import Camphor.NonEmpty
import Data.Maybe(isJust,listToMaybe)
import Text.Parsec  
import qualified Data.Map as M 
import Control.Monad.State
import Control.Monad.Reader
import Camphor.TupleTrans

unwrapAllMay :: [Value] -> Maybe [Ident2]
unwrapAllMay vs = mapM unwrap vs
 where 
  unwrap (Var i) = Just i; unwrap _ = Nothing 

makeNewIdent :: Ident2 -> UserState -> SourcePos -> Either ParseError Ident2  
makeNewIdent ident stat pos = do
 let identEggs = [ new | n <- [1..] :: [Integer], let new = tmpIdent ident n, not (stat `containsAnyIdent` new)]
 maybeToEither (newErrorMessage(Message$"FIXME: code 0011")pos)(listToMaybe identEggs) 
 
type RCMEP = ReaderT (CollisionTable,Maybe TmpStat) (Either ParseError)
type SCMEP = StateT  (CollisionTable,Maybe TmpStat) (Either ParseError)
 
{-  -------------------------------------------------------------------------------
   ***************************
   * definition of replacer3 *
   ***************************
----------------------------------------------------------------------------------}  
replacer3 :: 
 UserState -> NonEmpty MacroId -> SourcePos -> SimpleSent2 -> ReplTable -> SCMEP [SimpleSent]
-- simple ones --
replacer3 _ _ _   R_Scolon     _     = return[Scolon]
replacer3 _ _ _   (R_Sp x)     _     = return[Sp x]
replacer3 _ _ _   (R_Comm x)   _     = return[Comm x]
replacer3 _ _ pos (R_Pragma x) table = toState $ case x of
 ("MEMORY":"using":vars) -> do -- replaces the ARGUMENT of `using' pragma ...*1
  let vars' = forM vars toIdent2
  case vars' of 
   Left e -> err $ newErrorMessage(Message$showStr e++" used in a `MEMORY using' pragma is not an identifier")pos
   Right vars_ -> do
    replaced <- replaceSingles table (map Var vars_)
    let censored = unwrapAllMay replaced
    case censored of 
     Nothing  -> err $ newErrorMessage(Message$"something used in a `MEMORY using' pragma is not a variable")pos
     Just res -> return[Pragma$"MEMORY":"using":map unId res] 
 _                       -> return[Pragma x] 

{- 
 *1 when the function `void fnc(char& a){ char b; char c; /*# MEMORY using b c#*/dowith(a) }'
     is called with a variable d, it has to be 
	 { char b__TMP_1; char c__TMP_1; /*# MEMORY using b__TMP_1 c__TMP_1*/dowith(d) }
	 -} 
 
-- errors --
replacer3 _ _ pos (R_Infl _ _         ) _  = err$newErrorMessage(Message "cannot declare fixity inside function/operator definition ")pos  
replacer3 _ _ pos (R_Infr _ _         ) _  = err$newErrorMessage(Message "cannot declare fixity inside function/operator definition ")pos  
replacer3 _ _ pos (R_Func1 ident _ _  ) _  = err$cantdefine("function " ++ showStr(unId ident))pos
replacer3 _ _ pos (R_Func1Nul ident _ ) _  = err$cantdefine("function " ++ showStr(unId ident))pos 
replacer3 _ _ pos (R_Func2 oper _ _ _ ) _  = err$cantdefine("operator " ++ showStr(unOp oper ))pos 
replacer3 _ _ pos (R_Func2Nul oper _ _) _  = err$cantdefine("operator " ++ showStr(unOp oper ))pos 
 
-- char & delete -- 
replacer3 stat _ pos (R_Char ident) table  = case M.lookup ident table of
 Just _  -> err$newErrorMessage(Message$"cannot redefine an argument "++showStr(unId ident))pos 
 Nothing -> do
  (clTable,using) <- get
  if ident `M.member` clTable then err$newErrorMessage(Message$"dual definition of variable "++showStr(unId ident))pos else return()
  case using of {
   Nothing     -> do{newIdent <- lift(makeNewIdent ident stat pos); modifyFst (M.insert ident (newIdent,False)); return [Char newIdent]};
   Just []     -> do{newIdent <- lift(makeNewIdent ident stat pos); modifyFst (M.insert ident (newIdent,False)); return [Char newIdent]};
   Just (u:us) -> do{putSnd(Just us); newIdent <- return u;         modifyFst (M.insert ident (newIdent,True )); return [Comm$"char "++unId u++";"]};
  }
  
replacer3 _ _ pos (R_Del ident) table  = case M.lookup ident table of 
 Just _  -> err$newErrorMessage(Message$"cannot delete an argument"++showStr(unId ident))pos 
 Nothing -> do
  clTable <- getFst
  (newIdent,isUsing) <- lift $ maybeToEither (newErrorMessage(Message$"variable "++showStr(unId ident)++" is not defined")pos) (M.lookup ident clTable)
  modifyFst (M.delete ident)
  return [if isUsing then Comm$"delete " ++ unId newIdent ++ ";" else Del newIdent] --can't delete the one passed by `using' pragma

-- calls --  
replacer3 stat (n:|ns) pos (R_Call1 ident valuelist) table = toState $ call1 stat (n:|ns) pos (ident,valuelist) table
replacer3 stat (n:|ns) pos (R_Call1WithBlock ident valuelist pos2 block) table = toState $ do  -- ident is not replaced
 newValuelist <- replaceSingles table valuelist
 newblock <- makeNewBlock (Block pos2 block)
 return [Call1WithBlock ident newValuelist pos2 newblock]
 where
  makeNewBlock :: Sent -> RCMEP Sents
  makeNewBlock sent  = do
   sents <- fromState $ makeNewBlock2 sent 
   return sents
  
  makeNewBlock2 :: Sent -> SCMEP Sents
  makeNewBlock2 (Single _ ssent) = do
   res <- replacer3 stat (n:|ns) pos (toSimpleSent2 ssent) table
   return $ toSents pos res
  makeNewBlock2 (Block  _ xs) = do
   replaced <- forM xs makeNewBlock2 
   return $ concat replaced   

replacer3 stat (n:|ns) pos (R_Call2 oper valuelist1 valuelist2) table = toState $ call2 stat (n:|ns) pos (oper,valuelist1,valuelist2) table

replacer3 stat narr pos (R_Call3 op valuelist1 valuelist2) table = toState $ do
 lift $ isValidCall3 pos op valuelist2 stat
 call2 stat narr pos (op,valuelist1,valuelist2) table

replacer3 stat narr pos (R_Call4 []     valuelist ) table = replacer3 stat narr pos ((R_Call5 valuelist)) table
replacer3 stat narr pos (R_Call4 (x:xs) valuelist2) table = toState $ do
 (valuelist1,op) <- lift $ getCall4Left pos (x:|xs) stat
 call2 stat narr pos (op,valuelist1,valuelist2) table

replacer3 _ _ _ (R_Call5 (SepList(Constant _,[]))) _      = return[Scolon]
replacer3 _ _ _ (R_Call5 (SepList(Var ident ,[]))) table  = toState$ case M.lookup ident table of
 Nothing -> do{clt <- askFst;return[Call5(SepList(Var$newIdentIP clt ident,[]))]}
 Just x  -> return[Call5(SepList(x,[]))]
replacer3 stat narr pos ((R_Call5 (SepList(x,ov:ovs)))) table = toState $ do
 (oper,vlist1,vlist2) <- lift $ getCall5Result pos (x,ov:|ovs) stat
 call2 stat narr pos (oper,vlist1,vlist2) table
 
--- built-in (+=) & (-=) ---
replacer3 _ _ pos (R_Pleq (Var v1) (Constant c)) table  = toState $ case M.lookup v1 table of
 Nothing        -> do{clt <- askFst; return[Pleq (newIdentIP clt v1) c ]}
 Just (Var v)   -> return[Pleq v c]
 Just _         -> err$cantbeleft (unId v1) "+=" pos 

replacer3 _ _ pos (R_Mneq (Var v1) (Constant c)) table  = toState $ case M.lookup v1 table of
 Nothing        -> do{clt <- askFst; return[Mneq (newIdentIP clt v1) c]}
 Just (Var v)   -> return[Mneq v c]
 Just _         -> err$cantbeleft (unId v1) "-=" pos  

replacer3 _ _ pos (R_Pleq (Constant c) _) _ = err$(cantbeleft_ c "+=")pos  
replacer3 _ _ pos (R_Mneq (Constant c) _) _ = err$(cantbeleft_ c "-=")pos  
replacer3 stat ns pos ((R_Pleq (Var v1) (Var v2))) table = toState $ basis Pleq "+=" (stat,ns,pos,v1,v2,table)
replacer3 stat ns pos ((R_Mneq (Var v1) (Var v2))) table = toState $ basis Mneq "-=" (stat,ns,pos,v1,v2,table)

--- built-in read() & write() ---
replacer3 stat ns pos (R_Rd  c@(Constant _)) table = replacer3 stat ns pos ((R_Call1 readI  (SepList (c,[])))) table
replacer3 stat ns pos (R_Wrt c@(Constant _)) table = replacer3 stat ns pos ((R_Call1 writeI (SepList (c,[])))) table

replacer3 stat ns pos (R_Rd (Var ident)) table = toState $ case M.lookup ident table of
 Nothing      -> do{clt <- askFst; return[Rd (newIdentIP clt ident)]}
 Just (Var v) -> return[Rd v] 
 Just c       -> call1 stat ns pos (readI,(SepList (c,[]))) table 

replacer3 stat ns pos (R_Wrt (Var ident)) table = toState $ case M.lookup ident table of
 Nothing      -> do{clt <- askFst; return[Wrt (newIdentIP clt ident)]}
 Just (Var v) -> return[Wrt v] 
 Just c       -> call1 stat ns pos (writeI,(SepList (c,[]))) table 
 
{-  -------------------------------------------------------------------------------
   ********************
   * end of replacer3 *
   ********************
----------------------------------------------------------------------------------}
call1 :: UserState -> NonEmpty MacroId -> SourcePos -> (Ident2, ValueList) -> ReplTable -> RCMEP [SimpleSent]
call1 stat (n:|ns) pos (ident,valuelist) table
 | isJust$ M.lookup ident table = err$newErrorMessage(Message$"cannot call an argument "++showStr(unId ident))pos 
 | otherwise = do
  let matchingInstance = [ a | a@(Func name (typelist,_)) <- (n:ns), name == ident , valuelist `matches` typelist]
  case matchingInstance of
   []    -> rpl3 (n:|ns) pos ident valuelist table stat
   (x:_) -> err$newErrorMessage(Message$"cannot call "++show' x++" recursively inside "++show' n)pos

call2 :: UserState -> NonEmpty MacroId -> SourcePos -> (Oper, SepList Oper Value, SepList Oper Value) -> ReplTable -> RCMEP [SimpleSent]
call2 stat (n:|ns) pos (oper,valuelist1,valuelist2) table = do
 let matchingInstance = [ a | a@(Operator o (typelist1,typelist2,_)) <- (n:ns), o == oper, valuelist1 `matches` typelist1, valuelist2 `matches` typelist2 ]
 case matchingInstance of
   []    -> rpl4 (n:|ns) pos oper (valuelist1,valuelist2) table stat
   (x:_) -> err$newErrorMessage(Message$"cannot call "++show' x++" recursively inside "++show' n)pos  
 
-- replaces `variable += variable' or `variable -= variable' 
basis :: (Ident2 -> Integer -> SimpleSent) -> String -> 
  (UserState, NonEmpty MacroId, SourcePos, Ident2, Ident2, M.Map Ident2 Value) -> RCMEP [SimpleSent]
basis constr op (stat,ns,pos,v1,v2,table) = case M.lookup v1 table of
 Nothing      -> do{clt <- askFst; res(newIdentIP clt v1)} 
 Just (Var v) -> res v 
 Just _       -> err$cantbeleft (unId v1) op pos 
 where
 res :: Ident2 -> RCMEP [SimpleSent]
 res v = case M.lookup v2 table of
  Nothing           -> do{clt <- askFst; call2 stat ns pos ((wrap op),(SepList(Var v,[])),(SepList(Var$newIdentIP clt v2,[]))) table}
  Just (Var y)      -> call2 stat ns pos ((wrap op),(SepList(Var v,[])),(SepList(Var y            ,[]))) table
  Just (Constant c) -> return[constr v c]

  

--- First, replace valuelist by the collision table. Then, replace valuelist by the replacement table. Then, expand the macro.
--       outermacros         position    name     valuelist    replacement table                          collision table
rpl3 :: NonEmpty MacroId -> SourcePos -> Ident2 -> ValueList -> ReplTable -> UserState -> RCMEP [SimpleSent]
rpl3 ms pos ident valuelist table stat = do
 newValuelist <- replaceSingles table valuelist -- replacement of valuelist
 (typelist,sent') <- lift $ getInstanceOfCall1 pos ident newValuelist stat 
 case sent' of 
  Nothing -> err$newErrorMessage(Message$"cannot call function "++showStr(unId ident)++" because it is defined as null")pos ;
  Just sent -> do -- obtain what's in the function
   let mname = Func ident (typelist,Just sent) 
   let table2 = makeReplacerTable typelist newValuelist -- replace the parameters using the arguments
   sents <- rpl1_1 pos (toSent2 sent) table2 (mname `cons` ms) stat
   return sents
 
--- First, replace valuelist by the collision table. Then, replace valuelist by the replacement table. Then, expand the macro.
rpl4 ::
 NonEmpty MacroId -> SourcePos -> Oper -> (ValueList,ValueList) -> ReplTable -> UserState -> 
 RCMEP [SimpleSent]
rpl4 ms pos oper (vlist1,vlist2) table stat = do
 newVlist1 <- replaceSingles table vlist1 -- replacement of valuelist 
 newVlist2 <- replaceSingles table vlist2 -- replacement of valuelist 
 (tlist1,tlist2,sent') <- lift $ getInstanceOfCall2 pos oper newVlist1 newVlist2 stat
 case sent' of 
  Nothing -> err$newErrorMessage(Message$"cannot call operator "++unOp oper++" because it is defined as null")pos 
  Just sent -> do
   let mname = Operator oper (tlist1,tlist2,Just sent) 
   let table2 = makeReplacerTable2 (tlist1,tlist2) (newVlist1,newVlist2)
   sents <- rpl1_1 pos sent table2 (mname `cons` ms) stat 
   return sents

--- replaces a valuelist with collision table and replacement table   
replaceSingles :: Functor f => ReplTable -> f Value -> RCMEP (f Value)
replaceSingles table vlist = do
 coltable <- askFst
 return $ fmap (replaceSingle table . replaceSingleCollision coltable) vlist
 where
  replaceSingleCollision :: CollisionTable -> Value -> Value
  replaceSingleCollision _    m@(Constant _) = m
  replaceSingleCollision ctbl v@(Var idn)    = case M.lookup idn ctbl of Nothing -> v; Just (val,_) -> Var val
 
--- simply replace the parameters using the arguments  
rpl1_1 :: SourcePos -> Sent2 -> ReplTable -> NonEmpty MacroId -> UserState -> RCMEP [SimpleSent]
rpl1_1 pos sent table2 newMs stat = ReaderT $ evalStateT (rpl1_2 pos sent table2 newMs stat) 
 -- we outsiders don't need to know what's in the insiders' collision table.

--- simply replace the parameters using the arguments  
rpl1_2 :: SourcePos -> Sent2 -> ReplTable -> NonEmpty MacroId -> UserState -> SCMEP [SimpleSent]
rpl1_2 pos (Single _ ssent) table2 newMs stat = replacer3 stat newMs pos ssent table2 -- replace the inner block
rpl1_2 pos (Block  _ xs)    table2 newMs stat = do
 results <- forM xs (\ssent -> rpl1_2 pos ssent table2 newMs stat)
 return(concat results)

replaceSingle :: ReplTable -> Value -> Value
replaceSingle _     m@(Constant _) = m
replaceSingle table v@(Var idn)    = case M.lookup idn table of Nothing -> v; Just val -> val  

newIdentIP :: CollisionTable -> Ident2 -> Ident2 --newIdentIfpossible 
newIdentIP clt ident = case M.lookup ident clt of Nothing -> ident; Just (x,_) -> x 
 
-- messages --
cantdefine :: String -> SourcePos -> ParseError
cantdefine d = newErrorMessage(Message$"cannot define "++d++"inside function/operator definition ")

cantbeleft :: String -> String -> SourcePos -> ParseError
cantbeleft c str = newErrorMessage(Message$showStr c++" is a constant and thus cannot be the left side of operator "++showStr str) 

cantbeleft_ :: (Num a,Show a) => a -> String -> SourcePos -> ParseError
cantbeleft_ c str = newErrorMessage(Message$showNum c++" is a constant and thus cannot be the left side of operator "++showStr str) 