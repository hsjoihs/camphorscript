{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base.Base_Step2.Replacer2
(replacer3
)where
import Camphor.SafePrelude 
import Camphor.SepList(SepList(..)) 
import Camphor.Base.Base_Step2.Type
import Camphor.Base.Base_Step2.UserState
import Camphor.Base.Base_Step2.Auxilary2
import Camphor.Base.Base_Step2.Auxilary
import Camphor.Base.Base_Step2.Call5Result
import Camphor.Base.Base_Step2.ErrList
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import Camphor.NonEmpty
import Text.Parsec  
import qualified Data.Map as M 
import Control.Monad.State
import Control.Monad.Reader
import Camphor.TupleTrans
import Camphor.TailSepList

unwrapAllMay :: [Value] -> Either (NonEmpty Integer) [Ident2]
unwrapAllMay []              = Right []
unwrapAllMay (Var i:vs)      = (i:) <$> unwrapAllMay vs
unwrapAllMay (Constant c:vs) = case unwrapAllMay vs of Right _ -> Left(nE c); Left cs -> Left$ c `cons` cs


makeNewIdent :: CollisionTable -> Ident2 -> UserState -> SourcePos -> Either ParseError Ident2  
makeNewIdent clTable ident stat pos = do
 let identEggs = [ new | n <- [1..] :: [Integer], let new = tmpIdent ident n, not (stat `containsAnyIdent` new), Just new /= lookup2 ]
 maybeToEither (newErrorMessage(Message$"FIXME: code 0011")pos)(listToMaybe identEggs) 
 where
  lookup2 :: Maybe Ident2
  lookup2 = fmap fst $ M.lookup ident clTable

type RCMEP = ReaderT (CollisionTable,Maybe TmpStat,Maybe ()) (Either ParseError)
type SCMEP = StateT  (CollisionTable,Maybe TmpStat,Maybe ()) (Either ParseError)
 
{-  -------------------------------------------------------------------------------
   ***************************
   * definition of replacer3 *
   ***************************
----------------------------------------------------------------------------------}  
replacer3 :: 
 UserState -> NonEmpty MacroId -> SourcePos -> SimpleSent2 -> ReplTable -> SCMEP [Sent]
-- simple ones --
replacer3 _ _ pos R_Scolon     _     = return[Single pos $ Scolon]
replacer3 _ _ pos (R_Sp x)     _     = return[Single pos $ Sp x]
replacer3 _ _ pos (R_Comm x)   _     = return[Single pos $ Comm x]
replacer3 _ _ pos (R_Pragma x) table = toState $ case x of
 ("MEMORY":"using":vars) -> do -- replaces the ARGUMENT of `using' pragma ...*1
  let vars' = forM vars toIdent2
  case vars' of 
   Left e -> err $toPE pos$ Step2 <!> Prag <!> Memory <!> Using <!> NotValidIdent e
   Right vars_ -> do
    replaced <- replaceSingles table (map Var vars_)
    case unwrapAllMay replaced of 
     Left fails -> err $toPE pos$ Step2 <!> Prag <!> Memory <!> Using <!> IsConstant fails
     Right res  -> return[Single pos $ Pragma$"MEMORY":"using":map unId res] 
 _                       -> return[Single pos $ Pragma x] 

{- 
 *1 when the function `void fnc(char& a){ char b; char c; /*# MEMORY using b c#*/dowith(a) }'
     is called with a variable d, it has to be 
	 { char b__TMP_1; char c__TMP_1; /*# MEMORY using b__TMP_1 c__TMP_1*/dowith(d) }
	 -} 
 
-- errors --
replacer3 _ _ pos (R_Infl _ o          ) _  = err$toPE pos$ Step2 <!> Fixity <!> WrongDef_2 <!> InsideCall_2 <!> Operat_3 o
replacer3 _ _ pos (R_Infr _ o          ) _  = err$toPE pos$ Step2 <!> Fixity <!> WrongDef_2 <!> InsideCall_2 <!> Operat_3 o 
replacer3 _ _ pos (R_Func1 ident _ _   ) _  = err$toPE pos$ Step2 <!> Type <!> WrongDef <!> InsideCall <!> Functi ident
replacer3 _ _ pos (R_Func1Nul ident _  ) _  = err$toPE pos$ Step2 <!> Type <!> WrongDef <!> InsideCall <!> Functi ident
replacer3 _ _ pos (R_Func2 oper _ _ _  ) _  = err$toPE pos$ Step2 <!> Type <!> WrongDef <!> InsideCall <!> Operat oper
replacer3 _ _ pos (R_Func2Nul oper _ _ ) _  = err$toPE pos$ Step2 <!> Type <!> WrongDef <!> InsideCall <!> Operat oper
replacer3 _ _ pos (R_Syntax1 name _ _  ) _  = err$toPE pos$ Step2 <!> Type <!> WrongDef <!> InsideCall <!> Synt name
replacer3 _ _ pos (R_Syntax2 name _ _  ) _  = err$toPE pos$ Step2 <!> Type <!> WrongDef <!> InsideCall <!> Synt name
 
-- char & delete -- 
replacer3 stat _ pos (R_Char ident) table  = case M.lookup ident table of
 Just _  -> err$toPE pos$ Step2 <!> Access <!> WrongDef_3 <!> Definedasarg_3 <!> Idn ident 
 Nothing -> do
  clTable <- getFst
  using <- getSnd
  if ident `M.member` clTable 
   then err$toPE pos$ Step2 <!> Access <!> WrongDef_3 <!> Alreadydefined <!> Idn ident 
   else return()
  case using of {
   Just (u:us) -> do{putSnd(Just us); newIdent <- return u;                 modifyFst (M.insert ident (newIdent,True )); return [Single pos $ Comm$"char "++unId u++";"]};
   _           -> do{newIdent <- lift(makeNewIdent clTable ident stat pos); modifyFst (M.insert ident (newIdent,False)); return [Single pos $ Char newIdent]};
  }
  
replacer3 _ _ pos (R_Del ident) table  = case M.lookup ident table of 
 Just _  -> err$toPE pos$ Step2 <!> Access <!> WrongDel <!> Definedasarg_2 <!> Idn ident 
 Nothing -> do
  clTable <- getFst
  (newIdent,isUsing) <- lift $ maybeToEither (toPE pos$ Step2 <!> Access <!> WrongDel <!> Notdefinedhere <!> Idn ident)(M.lookup ident clTable)
  modifyFst (M.delete ident)
  return [Single pos $ if isUsing then Comm$"delete " ++ unId newIdent ++ ";" else Del newIdent] --can't delete the one passed by `using' pragma

-- calls --  
replacer3 stat narr pos (R_Call1 ident valuelist) table = toState $ call1 stat narr pos (ident,valuelist) table
replacer3 stat narr pos (R_Call2 oper valuelist1 valuelist2) table = toState $ call2 stat narr pos (oper,valuelist1,valuelist2) table
replacer3 stat narr pos (R_Call3 op valuelist1 valuelist2) table = toState $ do
 lift $ isValidCall3 pos op valuelist2 stat
 call2 stat narr pos (op,valuelist1,valuelist2) table

replacer3 stat narr pos (R_Call4 []     valuelist ) table = replacer3 stat narr pos ((R_Call5 valuelist)) table
replacer3 stat narr pos (R_Call4 (x:xs) valuelist2) table = toState $ do
 (valuelist1,op) <- lift $ getCall4Left pos (x:|xs) stat
 call2 stat narr pos (op,valuelist1,valuelist2) table

replacer3 _ _ pos (R_Call5 (SepList(Constant _)[])) _      = return[Single pos $ Scolon]
replacer3 _ _ pos (R_Call5 (SepList(Var ident )[])) table  = toState $ do
 clt <- askFst
 let x = replaceSingle table clt (Var ident)
 return[Single pos $ Call5(return x)]
replacer3 stat narr pos ((R_Call5 (SepList x (ov:ovs)))) table = toState $ do
 (oper,vlist1,vlist2) <- lift $ getCall5Result pos (x,ov:|ovs) stat
 call2 stat narr pos (oper,vlist1,vlist2) table
 
--- built-in (+=) & (-=) ---
replacer3 _ _ pos (R_Pleq (Var v1) (Constant c)) table  = toState $ do
 clt <- askFst
 let x = replaceSingle table clt (Var v1)
 case x of
  Var v -> return[Single pos $ Pleq v c]
  _     -> err$toPE pos $ Step2 <!> Type <!> WrongCall <!> Leftofbuiltin "+=" <!> Variab v1

replacer3 _ _ pos (R_Mneq (Var v1) (Constant c)) table  = toState $ do
 clt <- askFst
 let x = replaceSingle table clt (Var v1)
 case x of
  Var v -> return[Single pos $ Mneq v c]
  _     -> err$toPE pos $ Step2 <!> Type <!> WrongCall <!> Leftofbuiltin "-=" <!> Variab v1

replacer3 _ _ pos (R_Pleq (Constant c) _) _ = err$toPE pos $ Step2 <!> Type <!> WrongCall <!> Leftofbuiltin "+=" <!> Consta c 
replacer3 _ _ pos (R_Mneq (Constant c) _) _ = err$toPE pos $ Step2 <!> Type <!> WrongCall <!> Leftofbuiltin "-=" <!> Consta c 
replacer3 stat ns pos ((R_Pleq (Var v1) (Var v2))) table = toState $ basis Pleq "+=" (stat,ns,pos,v1,v2,table)
replacer3 stat ns pos ((R_Mneq (Var v1) (Var v2))) table = toState $ basis Mneq "-=" (stat,ns,pos,v1,v2,table)

--- built-in read() & write() ---
replacer3 stat ns pos (R_Rd  c@(Constant _)) table = replacer3 stat ns pos ((R_Call1 readI  $return c )) table
replacer3 stat ns pos (R_Wrt c@(Constant _)) table = replacer3 stat ns pos ((R_Call1 writeI $return c )) table

replacer3 stat ns pos (R_Rd (Var ident)) table = toState $ do
 clt <- askFst
 let x = replaceSingle table clt (Var ident)
 case x of
  Var v -> return[Single pos $ Rd v] 
  c     -> call1 stat ns pos (readI,return c) table 

replacer3 stat ns pos (R_Wrt (Var ident)) table = toState $ do
 clt <- askFst
 let x = replaceSingle table clt (Var ident)
 case x of
  Var v -> return[Single pos $ Wrt v] 
  c     -> call1 stat ns pos (writeI,return c) table 

--- Syntax Calls ---
replacer3 stat narr pos (R_SynCall1 ident valuelist pos2 block) table 
 | isJust$ M.lookup ident table = err$toPE pos$ Step2 <!> Type <!> WrongCall <!> Definedasarg <!> Synt_2 ident 
 | otherwise = toState $ do  -- FIXME: syntax is not replaced
  newValuelist <- replaceSingles table valuelist
  newblock <- fromState $ makeNewBlock2(Block pos2 block)
  return [Single pos $ SynCall1 ident newValuelist pos2 newblock] 
  where
   makeNewBlock2 :: Sent -> SCMEP Sents
   makeNewBlock2 (Single _ ssent) = do
    res <- replacer3 stat narr pos (toSimpleSent2 ssent) table
    return res
   makeNewBlock2 (Block  _ xs) = do
    replaced <- forM xs makeNewBlock2 
    return $ concat replaced
   

replacer3 stat narr pos (R_SynCall2 ident tvaluelist pos2 block) table  
 | isJust$ M.lookup ident table = err$toPE pos$ Step2 <!> Type <!> WrongCall <!> Definedasarg <!> Synt_2 ident 
 | otherwise = toState $ do  -- FIXME: syntax is not replaced
  newTValuelist <- replaceSingles2 table tvaluelist
  newblock <- fromState $ makeNewBlock2(Block pos2 block)
  return [Single pos $ SynCall2 ident newTValuelist pos2 newblock] 
  where
   makeNewBlock2 :: Sent -> SCMEP Sents
   makeNewBlock2 (Single _ ssent) = do
    res <- replacer3 stat narr pos (toSimpleSent2 ssent) table
    return res
   makeNewBlock2 (Block  _ xs) = do
    replaced <- forM xs makeNewBlock2 
    return $ concat replaced
   
replacer3 _ _ pos R_SynBlock   _     = do
 
 return[Single pos $ SynBlock]

  
{-  -------------------------------------------------------------------------------
   ********************
   * end of replacer3 *
   ********************
----------------------------------------------------------------------------------}
call1 :: UserState -> NonEmpty MacroId -> SourcePos -> (Ident2, ValueList) -> ReplTable -> RCMEP [Sent]
call1 stat (n:|ns) pos (ident,valuelist) table
 | isJust$ M.lookup ident table = err$toPE pos$ Step2 <!> Type <!> WrongCall <!> Definedasarg <!> Functi_2 ident 
 | otherwise = do
  let matchingInstance = [ a | a@(Func name (typelist,_)) <- (n:ns), name == ident , valuelist `matches` typelist]
  case matchingInstance of
   []    -> rpl3 (n:|ns) pos ident valuelist table stat
   (x:_) -> err$toPE pos$Step2 <!> Type <!> WrongCall <!> Recursivecall n x

call2 :: UserState -> NonEmpty MacroId -> SourcePos -> (Oper, SepList Oper Value, SepList Oper Value) -> ReplTable -> RCMEP [Sent]
call2 stat (n:|ns) pos (oper,valuelist1,valuelist2) table = do
 let matchingInstance = [ a | a@(Operator o (typelist1,typelist2,_)) <- (n:ns), o == oper, valuelist1 `matches` typelist1, valuelist2 `matches` typelist2 ]
 case matchingInstance of
   []    -> rpl4 (n:|ns) pos oper (valuelist1,valuelist2) table stat
   (x:_) -> err$toPE pos$Step2 <!> Type <!> WrongCall <!> Recursivecall n x 
 
-- replaces `variable += variable' or `variable -= variable' 
basis :: (Ident2 -> Integer -> SimpleSent) -> String -> 
  (UserState, NonEmpty MacroId, SourcePos, Ident2, Ident2, M.Map Ident2 Value) -> RCMEP [Sent]
basis constr op (stat,ns,pos,v1,v2,table) = do
 clt <- askFst
 let x = replaceSingle table clt (Var v1)
 let k = replaceSingle table clt (Var v2)
 case (x,k) of
  (Var v,Var y)      -> call2 stat ns pos (wrap op,return(Var v),return(Var y)) table
  (Var v,Constant c) -> return[Single pos $ constr v c] 
  _                  -> err$toPE pos $ Step2 <!> Type <!> WrongCall <!> Leftofbuiltin op <!> Variab v1


--- First, replace valuelist by the collision table. Then, replace valuelist by the replacement table. Then, expand the macro.
--       outermacros         position    name     valuelist    replacement table                          collision table
rpl3 :: NonEmpty MacroId -> SourcePos -> Ident2 -> ValueList -> ReplTable -> UserState -> RCMEP [Sent]
rpl3 ms pos ident valuelist table stat = do
 newValuelist <- replaceSingles table valuelist -- replacement of valuelist
 (typelist,sent') <- lift $ getInstanceOfCall1 pos ident newValuelist stat 
 case sent' of 
  Nothing -> err$toPE pos$Step2 <!> Type <!> WrongCall <!> Nulldefined <!> Functi_4 ident;
  Just sent -> do -- obtain what's in the function
   let mname = Func ident (typelist,Just sent) 
   let table2 = makeReplacerTable typelist newValuelist -- replace the parameters using the arguments
   sents <- rpl1_1 pos (toSent2 sent) table2 (mname `cons` ms) stat
   return sents
 
--- First, replace valuelist by the collision table. Then, replace valuelist by the replacement table. Then, expand the macro.
rpl4 ::
 NonEmpty MacroId -> SourcePos -> Oper -> (ValueList,ValueList) -> ReplTable -> UserState -> 
 RCMEP [Sent]
rpl4 ms pos oper (vlist1,vlist2) table stat = do
 newVlist1 <- replaceSingles table vlist1 -- replacement of valuelist 
 newVlist2 <- replaceSingles table vlist2 -- replacement of valuelist 
 (tlist1,tlist2,sent') <- lift $ getInstanceOfCall2 pos oper newVlist1 newVlist2 stat
 case sent' of 
  Nothing -> err$toPE pos$Step2 <!> Type <!> WrongCall <!> Nulldefined <!> Operat_4 oper
  Just sent -> do
   let mname = Operator oper (tlist1,tlist2,Just sent) 
   let table2 = makeReplacerTable2 (tlist1,tlist2) (newVlist1,newVlist2)
   sents <- rpl1_1 pos sent table2 (mname `cons` ms) stat 
   return sents

--- replaces a valuelist with collision table top -> replacement table -> collision table rest   
replaceSingles :: Functor f => ReplTable -> f Value -> RCMEP (f Value)
replaceSingles table vlist = do
 coltable <- askFst
 return $ fmap (replaceSingle table coltable) vlist
 
replaceSingles2 :: ReplTable -> TailValueList -> RCMEP TailValueList
replaceSingles2 table (TSL tvlist) = do
 coltable <- askFst
 return $ TSL[(o,replaceSingle table coltable v) | (o,v) <- tvlist]
  
--- replaces a value with collision table top -> replacement table -> collision table rest  
replaceSingle :: ReplTable -> CollisionTable -> Value -> Value 
replaceSingle table t value = (replaceSingleRepl table . replaceSingleCollisionTop t) value
-- stat `containsAnyIdent`
 where
  replaceSingleCollisionTop :: M.Map Ident2 (Ident2,Bool) -> Value -> Value
  replaceSingleCollisionTop _    m@(Constant _) = m
  replaceSingleCollisionTop cttp v@(Var idn)    = case M.lookup idn cttp of Nothing -> v; Just (val,_) -> Var val
  
  replaceSingleRepl :: ReplTable -> Value -> Value
  replaceSingleRepl _   m@(Constant _) = m
  replaceSingleRepl tbl v@(Var idn)    = case M.lookup idn tbl of Nothing -> v; Just val -> val 

 
--- simply replace the parameters using the arguments  
rpl1_1 :: SourcePos -> Sent2 -> ReplTable -> NonEmpty MacroId -> UserState -> RCMEP [Sent]
rpl1_1 pos sent table2 newMs stat = ReaderT $ evalStateT (do{putFst M.empty; rpl1_2 pos sent table2 newMs stat})
 -- we outsiders don't need to know what's in the insiders' collision table.

--- simply replace the parameters using the arguments  
rpl1_2 :: SourcePos -> Sent2 -> ReplTable -> NonEmpty MacroId -> UserState -> SCMEP [Sent]
rpl1_2 pos (Single _ ssent) table2 newMs stat = do
 replacer3 stat newMs pos ssent table2 -- replace the inner block
rpl1_2 pos (Block  p xs)    table2 newMs stat = do
 results <- forM xs (\ssent -> rpl1_2 pos ssent table2 newMs stat)
 return [Block p $ concat results]