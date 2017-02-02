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

unwrapAllMay :: [Value] -> Maybe [Ident]
unwrapAllMay vs = mapM unwrap vs
 where 
  unwrap (Var i) = Just i; unwrap _ = Nothing 
  
{-  -------------------------------------------------------------------------------
   ***************************
   * definition of replacer3 *
   ***************************
----------------------------------------------------------------------------------}  
replacer3 :: 
 UserState -> NonEmpty MacroId -> SourcePos -> SimpleSent -> ReplTable -> Maybe TmpStat -> CollisionTable -> Either ParseError ([SimpleSent],CollisionTable)
-- simple ones --
replacer3 _ _ _ Scolon   _ _    = wrp[Scolon]
replacer3 _ _ _ (Sp x)   _ _    = wrp[Sp x]
replacer3 _ _ _ (Comm x) _ _    = wrp[Comm x]
replacer3 _ _ _ (Pragma x) table _  = case x of
 ("MEMORY":"using":vars) -> \clt -> do -- replaces the ARGUMENT of `using' pragma ...*1
  let replaced = replaceSingles table clt (map Var vars)
  let censored = unwrapAllMay replaced
  case censored of 
   Nothing  -> return([Comm$"invalid pragma: "++show(unwords x)],clt)
   Just res -> return([Pragma$"MEMORY":"using":res],clt)  
 _                       -> wrp[Pragma x] 

{- 
 *1 when the function
     `void fnc(char& a){ char b; char c; /*# MEMORY using b c#*/dowith(a) }'
     is called with a variable d, it has to be 
	 { char b__TMP_1; char c__TMP_1; /*# MEMORY using b__TMP_1 c__TMP_1*/dowith(d) }
	 -} 
 
-- errors --
replacer3 _ _ pos (Infl _ _         ) _ _ = err$newErrorMessage(Message "cannot declare fixity inside function/operator definition ")pos  
replacer3 _ _ pos (Infr _ _         ) _ _ = err$newErrorMessage(Message "cannot declare fixity inside function/operator definition ")pos  
replacer3 _ _ pos (Func1 ident _ _  ) _ _ = err$cantdefine("function "++show ident)pos
replacer3 _ _ pos (Func1Nul ident _ ) _ _ = err$cantdefine("function "++show ident)pos 
replacer3 _ _ pos (Func2 oper _ _ _ ) _ _ = err$cantdefine("operator "++show oper )pos 
replacer3 _ _ pos (Func2Nul oper _ _) _ _ = err$cantdefine("operator "++show oper )pos 
 
-- char & delete -- 
replacer3 stat _ pos (Char ident) table _ = case M.lookup ident table of
 Just _  -> err$newErrorMessage(Message$"cannot redefine an argument "++show ident)pos 
 Nothing -> \clTable -> do
  if ident `M.member` clTable then Left$newErrorMessage(Message$"dual definition of variable "++show ident)pos else return()
  let identEggs = [ new | n <- [1..] :: [Integer], let new = ident ++ "__TMP_" ++ show n, not (stat `containsAnyIdent` new)]
  newIdent <- maybeToEither (newErrorMessage(Message$"FIXME: fatal error")pos)(listToMaybe identEggs) -- FIXME: fatal error
  return([Char newIdent],M.insert ident newIdent clTable)
  
replacer3 _ _ pos (Del ident) table _ = case M.lookup ident table of
 Just _  -> err$newErrorMessage(Message$"cannot delete an argument"++show ident)pos 
 Nothing -> \clTable -> do
  newIdent <- maybeToEither (newErrorMessage(Message$"variable "++show ident++" is not defined")pos) (M.lookup ident clTable)
  return([Del newIdent],M.delete ident clTable)

-- calls --  
replacer3 stat (n:|ns) pos (Call1 ident valuelist) table using 
 | isJust$ M.lookup ident table = err$newErrorMessage(Message$"cannot call an argument "++show ident)pos 
 | otherwise = do
  let matchingInstance = [ a | a@(Func name (typelist,_)) <- (n:ns), name == ident , valuelist `matches` typelist]
  case matchingInstance of
   []    -> rpl3 (n:|ns) pos ident valuelist table stat using 
   (x:_) -> err$newErrorMessage(Message$"cannot call "++show' x++" recursively inside "++show' n)pos

replacer3 stat (n:|ns) pos (Call1WithBlock ident valuelist pos2 block) table using = \coltable -> do  -- ident is not replaced
 let newValuelist = replaceSingles table coltable valuelist
 newblock <- makeNewBlock (Block pos2 block) coltable
 return([Call1WithBlock ident newValuelist pos2 newblock],coltable)
 where
  makeNewBlock :: Sent -> CollisionTable -> Either ParseError Sents
  makeNewBlock sent clt = fst <$> makeNewBlock2 sent clt
  
  makeNewBlock2 :: Sent -> CollisionTable -> Either ParseError (Sents,CollisionTable)
  makeNewBlock2 (Single _ ssent) = \clt -> do
   (res,nclt) <- replacer3 stat (n:|ns) pos ssent table using clt
   return(map(Single pos)(toList' res),nclt)
  makeNewBlock2 (Block  _ xs) = \clt -> do
   (replaced,nclt) <- forStatM xs makeNewBlock2 clt --sequence [makeNewBlock sent | sent <- xs] 
   return(concat replaced,nclt)   

replacer3 stat (n:|ns) pos (Call2 oper valuelist1 valuelist2) table using = do
 let matchingInstance = [ a | a@(Operator o (typelist1,typelist2,_)) <- (n:ns), o == oper, valuelist1 `matches` typelist1, valuelist2 `matches` typelist2 ]
 case matchingInstance of
   []    -> rpl4 (n:|ns) pos oper (valuelist1,valuelist2) table stat using 
   (x:_) -> err$newErrorMessage(Message$"cannot call "++show' x++" recursively inside "++show' n)pos
     

replacer3 stat narr pos (Call3 op valuelist1 valuelist2) table using = \coltable -> do
 isValidCall3 pos op valuelist2 stat
 replacer3 stat narr pos (Call2 op valuelist1 valuelist2) table using coltable

replacer3 stat narr pos (Call4 []     valuelist ) table using = replacer3 stat narr pos (Call5 valuelist) table using
replacer3 stat narr pos (Call4 (x:xs) valuelist2) table using = \coltable -> do
 (valuelist1,op) <- getCall4Left pos (x:|xs) stat
 replacer3 stat narr pos (Call2 op valuelist1 valuelist2) table using coltable

replacer3 _ _ _ (Call5 (SepList(Constant _,[]))) _ _ = wrp[Scolon]
replacer3 _ _ _ (Call5 (SepList(Var ident,[]))) table _ = case M.lookup ident table of
 Nothing -> \clt -> return([Call5(SepList(newIdentIP clt ident,[]))],clt)
 Just x  -> wrp[Call5(SepList(x                   ,[]))]
replacer3 stat narr pos (Call5 (SepList(x,ov:ovs))) table using = \coltable -> do
 (oper,vlist1,vlist2) <- getCall5Result pos (x,ov:|ovs) stat
 replacer3 stat narr pos (Call2 oper vlist1 vlist2) table using coltable
 
--- built-in (+=) & (-=) ---
replacer3 _ _ pos (Pleq (Var v1) (Constant v2)) table _ = case M.lookup v1 table of
 Nothing        -> \clt -> return([Pleq (newIdentIP clt v1)(Constant v2)],clt)
 Just v@(Var _) -> wrp[Pleq v (Constant v2)]
 Just _         -> err$cantbeleft v1 "+=" pos 

replacer3 _ _ pos (Mneq (Var v1) (Constant v2)) table _ = case M.lookup v1 table of
 Nothing        -> \clt -> return([Mneq (newIdentIP clt v1)(Constant v2)],clt)
 Just v@(Var _) -> wrp[Mneq v (Constant v2)]
 Just _         -> err$cantbeleft v1 "-=" pos  

replacer3 _ _ pos (Pleq (Constant c) _) _ _ = err$(cantbeleft c "+=")pos  
replacer3 _ _ pos (Mneq (Constant c) _) _ _ = err$(cantbeleft c "-=")pos  
replacer3 stat ns pos (Pleq (Var v1) (Var v2)) table using = basis Pleq "+=" (stat,ns,pos,v1,v2,table,using)
replacer3 stat ns pos (Mneq (Var v1) (Var v2)) table using = basis Mneq "-=" (stat,ns,pos,v1,v2,table,using)


--- built-in read() & write() ---
replacer3 stat ns pos (Rd  c@(Constant _)) table using = replacer3 stat ns pos (Call1 "read" (SepList (c,[]))) table using 
replacer3 stat ns pos (Wrt c@(Constant _)) table using = replacer3 stat ns pos (Call1 "write" (SepList (c,[]))) table using

replacer3 stat ns pos (Rd (Var ident)) table using = case M.lookup ident table of
 Nothing        -> \clt -> return([Rd (newIdentIP clt ident)],clt)
 Just v@(Var _) -> wrp[Rd v] 
 Just c         -> replacer3 stat ns pos (Call1 "read" (SepList (c,[]))) table using 

replacer3 stat ns pos (Wrt (Var ident)) table using = case M.lookup ident table of
 Nothing        -> \clt -> return([Wrt (newIdentIP clt ident)],clt)
 Just v@(Var _) -> wrp[Wrt v] 
 Just c         -> replacer3 stat ns pos (Call1 "write" (SepList (c,[]))) table using
 
{-  -------------------------------------------------------------------------------
   ********************
   * end of replacer3 *
   ********************
----------------------------------------------------------------------------------}
wrp :: (Monad m) => b -> a -> m (b,a)
wrp b a = return(b,a)

err :: a -> t -> Either a b
err a _ = Left a  
  
-- replaces `variable += variable' or `variable -= variable' 
basis :: (Value -> Value -> SimpleSent) -> String
 -> (UserState, NonEmpty MacroId, SourcePos, Ident, Ident, M.Map Ident Value, Maybe TmpStat)
 -> CollisionTable -> Either ParseError ([SimpleSent],CollisionTable)
basis constr op (stat,ns,pos,v1,v2,table,using) = case M.lookup v1 table of
 Nothing        -> \clt -> res (newIdentIP clt v1) clt
 Just v@(Var _) -> res v 
 Just _         -> err$cantbeleft v1 op pos 
 where
 res :: Value -> CollisionTable -> Either ParseError ([SimpleSent],CollisionTable)
 res v = case M.lookup v2 table of
  Nothing      -> \clt -> replacer3 stat ns pos (Call2 (wrap op) (SepList(v,[])) (SepList(newIdentIP clt v2,[]))) table using clt
  Just (Var y) -> \clt -> replacer3 stat ns pos (Call2 (wrap op) (SepList(v,[])) (SepList(Var y            ,[]))) table using clt
  Just c       -> wrp[constr v c]

  

--- First, replace valuelist by the collision table. Then, replace valuelist by the replacement table. Then, expand the macro.
--       outermacros         position    name     valuelist    replacement table                          collision table
rpl3 :: NonEmpty MacroId -> SourcePos -> Ident -> ValueList -> ReplTable -> UserState -> Maybe TmpStat -> CollisionTable -> Either ParseError ([SimpleSent],CollisionTable) 
rpl3 ms pos ident valuelist table stat using coltable = do
 let newValuelist = replaceSingles table coltable valuelist -- replacement of valuelist
 (typelist,sent') <- getInstanceOfCall1 pos ident newValuelist stat 
 case sent' of 
  Nothing -> Left$newErrorMessage(Message$"cannot call function "++show ident++" because it is defined as null")pos ;
  Just sent -> do -- obtain what's in the function
   let mname = Func ident (typelist,Just sent) ; table2 = makeReplacerTable typelist newValuelist -- replace the parameters using the arguments
   sents <- rpl1_1 pos sent table2 (mname `cons` ms) stat using coltable
   return(sents,coltable)
 
--- First, replace valuelist by the collision table. Then, replace valuelist by the replacement table. Then, expand the macro.
rpl4 ::
 NonEmpty MacroId -> SourcePos -> Oper -> (ValueList,ValueList) -> ReplTable -> UserState -> Maybe TmpStat -> CollisionTable -> Either ParseError ([SimpleSent],CollisionTable) 
rpl4 ms pos oper (vlist1,vlist2) table stat using coltable = do
 let newVlist1 = replaceSingles table coltable vlist1 -- replacement of valuelist
 let newVlist2 = replaceSingles table coltable vlist2 -- replacement of valuelist
 (tlist1,tlist2,sent') <- getInstanceOfCall2 pos oper newVlist1 newVlist2 stat
 case sent' of 
  Nothing -> Left$newErrorMessage(Message$"cannot call operator "++unOp oper++" because it is defined as null")pos 
  Just sent -> do
   let mname = Operator oper (tlist1,tlist2,Just sent) ; table2 = makeReplacerTable2 (tlist1,tlist2) (newVlist1,newVlist2)
   sents <- rpl1_1 pos sent table2 (mname `cons` ms) stat using coltable
   return(sents,coltable)

--- replaces a valuelist with collision table and replacement table   
replaceSingles :: Functor f => ReplTable -> CollisionTable -> f Value -> f Value
replaceSingles table coltable vlist = fmap (replaceSingle table) (fmap (replaceSingleCollision coltable) vlist)
 where
  replaceSingleCollision :: CollisionTable -> Value -> Value
  replaceSingleCollision _    m@(Constant _) = m
  replaceSingleCollision ctbl v@(Var idn)    = case M.lookup idn ctbl of Nothing -> v; Just val -> Var val
 
--- simply replace the parameters using the arguments  
rpl1_1 :: SourcePos -> Sent -> ReplTable -> NonEmpty MacroId -> UserState -> Maybe TmpStat -> CollisionTable -> Either ParseError [SimpleSent]
rpl1_1 pos sent table2 newMs stat using clt = 
 fst <$> rpl1_2 pos sent table2 newMs stat using clt  -- we outsiders don't need to know what's in the insiders' collision table.

 
--- simply replace the parameters using the arguments  
rpl1_2 :: SourcePos -> Sent -> ReplTable -> NonEmpty MacroId -> UserState -> Maybe TmpStat -> CollisionTable -> Either ParseError ([SimpleSent],CollisionTable)
rpl1_2 pos (Single _ ssent) table2 newMs stat using coltable = replacer3 stat newMs pos ssent table2 using coltable -- replace the inner block
rpl1_2 pos (Block  _ xs)    table2 newMs stat using coltable = do
 (results,clTable) <- forStatM xs (\ssent clt -> rpl1_2 pos ssent table2 newMs stat using clt) coltable
 return(concat results,clTable)
 {- forStatM is defined in Camphor.Global.Utilities as `forStatM :: (Monad m) => [a] -> (a -> b -> m (c,b)) -> b -> m ([c],b)' 
 -- and can be used to sequence with passing a state  -}
 
replaceSingle :: ReplTable -> Value -> Value
replaceSingle _     m@(Constant _) = m
replaceSingle table v@(Var idn)    = case M.lookup idn table of Nothing -> v; Just val -> val  

newIdentIP :: CollisionTable -> Ident -> Value --newIdentIfpossible 
newIdentIP clt ident = case M.lookup ident clt of Nothing -> Var ident; Just x -> Var x 
 
-- messages --
cantdefine :: [Char] -> SourcePos -> ParseError
cantdefine d = newErrorMessage(Message$"cannot define "++d++"inside function/operator definition ")

cantbeleft :: (Show a) => a -> String -> SourcePos -> ParseError
cantbeleft c str = newErrorMessage(Message$show c++" is a constant and thus cannot be the left side of operator "++show str) 