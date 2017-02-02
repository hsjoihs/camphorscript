{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base_Step2.Replacer2
(replacer2
)where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.SepList(SepList(..)) 
import Camphor.Base_Step2.Type
import Camphor.Base_Step2.UserState
import Camphor.Base_Step2.New 
import Camphor.Base_Step2.Auxilary
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import Camphor.NonEmpty
import Data.Maybe(isJust)
import Text.Parsec  
import qualified Data.Map as M 

{-  -------------------------------------------------------------------------------
   ***************************
   * definition of replacer2 *
   ***************************
----------------------------------------------------------------------------------}   
replacer2 :: UserState -> NonEmpty MacroId -> SourcePos -> SimpleSent -> ReplTable ->  Either ParseError (NonEmpty SimpleSent)

replacer2 _ _ _ Scolon   _ = return(nE Scolon)
replacer2 _ _ _ (Sp x)   _ = return(nE $ Sp x)
replacer2 _ _ _ (Comm x) _ = return(nE $ Comm x)
replacer2 _ _ pos (Infl _ _) _ = Left$newErrorMessage(Message$"cannot declare fixity inside function/operator definition ")pos  
replacer2 _ _ pos (Infr _ _) _ = Left$newErrorMessage(Message$"cannot declare fixity inside function/operator definition ")pos  

replacer2 _ _ pos (Char ident) table = case M.lookup ident table of
 Nothing -> return(nE $ Char ident)
 Just _  -> Left$newErrorMessage(Message$"cannot redefine an argument "++show ident)pos 
 
replacer2 _ _ pos (Del ident) table = case M.lookup ident table of
 Nothing -> return(nE $ Del ident)
 Just _  -> Left$newErrorMessage(Message$"cannot delete an argument"++show ident)pos 

replacer2 _ _ pos (Pleq (Var v1) (Constant v2)) table = case M.lookup v1 table of
 Nothing      -> return(nE $ Pleq (Var v1)(Constant v2))
 Just (Var x) -> return(nE $ Pleq (Var x) (Constant v2))
 Just _       -> Left $newErrorMessage(Message$show v1++" is a constant and thus cannot be the left side op operator \"+=\"")pos 
replacer2 stat ns pos (Pleq (Var v1) (Var v2)) table = case M.lookup v1 table of
 Nothing -> res v1
 Just (Var x) -> res x
 Just _       -> Left $newErrorMessage(Message$show v1++" is a constant and thus cannot be the left side op operator \"+=\"")pos 
 where
 res var = case M.lookup v2 table of
  Nothing      -> Left $newErrorMessage(Message$"FIXME:: code 0001")pos 
  Just (Var y) -> replacer2 stat ns pos (Call2 (wrap "+=") (SepList((Var var),[])) (SepList((Var y),[]))) table 
  Just c       -> return(nE $ Pleq (Var var) c)
replacer2 _ _ pos (Pleq (Constant c) _) _ = Left $newErrorMessage(Message$show c++" is a constant and thus cannot be the left side op operator \"+=\"")pos 
  
replacer2 _ _ pos (Mneq (Var v1) (Constant v2)) table = case M.lookup v1 table of
 Nothing      -> return(nE $ Mneq (Var v1)(Constant v2))
 Just (Var x) -> return(nE $ Mneq (Var x) (Constant v2))
 Just _       -> Left $newErrorMessage(Message$show v1++" is a constant and thus cannot be the left side op operator \"-=\"")pos 
replacer2 stat ns pos (Mneq (Var v1) (Var v2)) table = case M.lookup v1 table of
 Nothing -> res v1
 Just (Var x) -> res x
 Just _       -> Left $newErrorMessage(Message$show v1++" is a constant and thus cannot be the left side op operator \"-=\"")pos 
 where
 res var = case M.lookup v2 table of
  Nothing      -> Left $newErrorMessage(Message$"FIXME:: code 0001")pos 
  Just (Var y) -> replacer2 stat ns pos (Call2 (wrap "-=") (SepList((Var var),[])) (SepList((Var y),[]))) table 
  Just c       -> return(nE $ Mneq (Var var) c)
replacer2 _ _ pos (Mneq (Constant c) _) _ = Left $newErrorMessage(Message$show c++" is a constant and thus cannot be the left side op operator \"-=\"")pos 
 
replacer2 stat ns pos (Rd (Var ident)) table = case M.lookup ident table of
 Nothing     -> Left $newErrorMessage(Message$"identifier "++show ident++" is not defined")pos 
 Just(Var x) -> return(nE $ Rd (Var x)) 
 Just c      -> replacer2 stat ns pos (Call1 "read" (SepList (c,[]))) table 
replacer2 stat ns pos (Rd c) table = replacer2 stat ns pos (Call1 "read" (SepList (c,[]))) table
 
replacer2 stat ns pos (Wrt (Var ident)) table = case M.lookup ident table of
 Nothing     -> Left $newErrorMessage(Message$"identifier "++show ident++" is not defined")pos 
 Just(Var x) -> return(nE $ Wrt (Var x))  
 Just c      -> replacer2 stat ns pos (Call1 "write" (SepList (c,[]))) table 
replacer2 stat ns pos (Wrt c) table = replacer2 stat ns pos (Call1 "write" (SepList (c,[]))) table 

replacer2 _ _ pos (Func1 ident _ _) _ = 
 Left$newErrorMessage(Message$"cannot define function "++show ident++"inside function/operator definition ")pos
 
replacer2 _ _ pos (Func2 oper _ _ _) _ = 
 Left$newErrorMessage(Message$"cannot define operator "++show oper ++"inside function/operator definition ")pos 
  
replacer2 stat (n:|ns) pos (Call1 ident valuelist) table 
 | isJust$ M.lookup ident table = Left$newErrorMessage(Message$"cannot call an argument "++show ident)pos 
 | otherwise = do
  let matchingInstance = [ a | a@(Func name (typelist,_)) <- (n:ns), name == ident , valuelist `matches` typelist]
  case matchingInstance of
   []    -> rpl1 (n:|ns) pos ident valuelist table stat
   (x:_) -> Left$newErrorMessage(Message$"cannot call "++show' x++" recursively inside "++show' n)pos
   
replacer2 stat (n:|ns) pos (Call1WithBlock ident valuelist block) table = do  -- ident is not replaced
 let newValuelist = fmap (replaceSingle table) valuelist
 newblock <- makeNewBlock (Block block)
 return(nE $ Call1WithBlock ident newValuelist newblock)
 where
  makeNewBlock :: Sent -> Either ParseError Sents
  makeNewBlock (Single(_,ssent)) = do
   res <- replacer2 stat (n:|ns) pos ssent table
   return$map (\x->Single(pos,x))(toList res)
  makeNewBlock (Block xs) = do
   replaced <- sequence [makeNewBlock sent | sent <- xs] 
   return$ concat replaced

replacer2 stat (n:|ns) pos (Call2 oper valuelist1 valuelist2) table = do
 let matchingInstance = [ a | a@(Operator o (typelist1,typelist2,_)) <- (n:ns), o == oper, valuelist1 `matches` typelist1, valuelist2 `matches` typelist2 ]
 case matchingInstance of
   []    -> rpl2 (n:|ns) pos oper (valuelist1,valuelist2) table stat
   (x:_) -> Left$newErrorMessage(Message$"cannot call "++show' x++" recursively inside "++show' n)pos
  
replacer2 stat narr pos (Call3 op valuelist1 valuelist2) table = do
 isValidCall3 pos op valuelist2 stat
 replacer2 stat narr pos (Call2 op valuelist1 valuelist2) table

replacer2 stat narr pos (Call4 []     valuelist ) table = replacer2 stat narr pos (Call5 valuelist) table

replacer2 stat narr pos (Call4 (x:xs) valuelist2) table = do
 (valuelist1,op) <- getCall4Left pos (x:|xs) stat
 replacer2 stat narr pos (Call2 op valuelist1 valuelist2) table

replacer2 _ _ _ (Call5 (SepList(Constant _,[]))) _ = return(nE $ Scolon)
replacer2 _ _ _ (Call5 (SepList(Var ident,[]))) table = case M.lookup ident table of
 Nothing -> return (nE $ Call5 (SepList(Var ident,[])))
 Just x  -> return (nE $ Call5 (SepList(x        ,[])))
 
replacer2 stat narr pos (Call5 (SepList(x,ov:ovs))) table = do
 (oper,vlist1,vlist2) <- getCall5Result pos (x,ov:|ovs) stat
 replacer2 stat narr pos (Call2 oper vlist1 vlist2) table
{-  -------------------------------------------------------------------------------
   ********************
   * end of replacer2 *
   ********************
----------------------------------------------------------------------------------}

--- First, replace valuelist by the replacement table. Then, expand the macro.
--       outermacros         position    name     valuelist    replacement table
rpl1 :: NonEmpty MacroId -> SourcePos -> Ident -> ValueList -> ReplTable -> UserState -> Either ParseError (NonEmpty SimpleSent)
rpl1 ms pos ident valuelist table stat = do
 let newValuelist = fmap (replaceSingle table) valuelist
 instnce@(typelist,sent) <- getInstanceOfCall1 pos ident newValuelist stat
 let mname = Func ident instnce ; table2 = makeReplacerTable typelist newValuelist
 rpl1_1 pos sent table2 (mname `cons` ms) stat 
  
rpl1_1 :: SourcePos -> Sent -> ReplTable -> NonEmpty MacroId -> UserState -> Either ParseError (NonEmpty SimpleSent)
rpl1_1 pos (Single(_,ssent)) table2 newMs stat  = replacer2 stat newMs pos ssent table2
rpl1_1 pos (Block xs) table2 newMs stat = do
 replaced <- sequence [rpl1_1 pos ssent table2 newMs stat | ssent <- xs] -- [NonEmpty SimpleSent]
 case replaced of 
  [] -> return(nE $ Scolon)
  (r:rs) -> return$concat' (r:|rs)
   
rpl2 :: NonEmpty MacroId -> SourcePos -> Oper -> (ValueList,ValueList) -> ReplTable -> UserState -> Either ParseError (NonEmpty SimpleSent)
rpl2 ms pos oper (vlist1,vlist2) table stat = do
 let newVlist1 = fmap (replaceSingle table) vlist1
 let newVlist2 = fmap (replaceSingle table) vlist2
 instnce@(tlist1,tlist2,sent) <- getInstanceOfCall2 pos oper newVlist1 newVlist2 stat
 let mname = Operator oper instnce ; table2 = makeReplacerTable2 (tlist1,tlist2) (newVlist1,newVlist2)
 rpl1_1 pos sent table2 (mname `cons` ms) stat
 
replaceSingle :: ReplTable -> Value -> Value
replaceSingle _ m@(Constant _) = m
replaceSingle table v@(Var idn) = case M.lookup idn table of
 Nothing -> v
 Just val -> val  