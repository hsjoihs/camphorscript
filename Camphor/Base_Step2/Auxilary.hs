{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base_Step2.Auxilary
(getOpContents2
,canBeRightOf
,canBeLeftOf
,getOpFixity,getOpsFixities,getOpsFixities'
,makeReplacerTable,makeReplacerTable2,ReplTable
,isConsistent,contradiction
,NonEmptyValue
,breakBy',reverse''
,isValidCall3,getCall4Left,getInstanceOfCall1,getInstanceOfCall2
,getLastPos
) where
import Camphor.SafePrelude
import qualified Camphor.SepList as S
import Camphor.Base_Step2.Type
import Camphor.Base_Step2.UserState
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import Text.Parsec 
import Camphor.NonEmpty
import qualified Data.Map as M
import Data.Maybe(listToMaybe)

getLastPos :: Sent -> SourcePos
getLastPos (Single pos _) = pos
getLastPos (Block  p [] ) = p
getLastPos (Block  _ (x:xs)) = getLastPos $ last' (x:|xs)

getInstanceOfCall2 :: SourcePos -> Oper -> ValueList -> ValueList -> UserState -> Either ParseError OpInstance
getInstanceOfCall2 pos op valuelist1 valuelist2 stat = do
 opinfo <- opinfo'
 let matchingOpInstance = [ a | a@(typelist1,typelist2,_) <- opinfo, valuelist1 `matches` typelist1, valuelist2 `matches` typelist2 ] 
 case matchingOpInstance of 
  []        -> Left $newErrorMessage(Message$"no type-matching instance of "++show op++" defined")pos 
  [instnce] -> return instnce
  xs        -> Left $newErrorMessage(Message$show(length xs)++" type-matching instances of "++show op++" defined")pos 
 where
  opinfo' :: Either ParseError [OpInstance]
  opinfo' = fmap snd $ getOpContents2 pos stat op

getInstanceOfCall1 :: SourcePos -> Ident -> ValueList -> UserState -> Either ParseError VFInstance
getInstanceOfCall1 pos ident valuelist stat = do
 finfo <- finfo'
 let matchingFuncInstance = [ a | a@(typelist,_) <- finfo, valuelist `matches` typelist ]
 case matchingFuncInstance of
  []        -> Left $newErrorMessage(Message$"no type-matching instance of "++show ident++" defined")pos  
  [instnce] -> return instnce
  xs        -> Left $newErrorMessage(Message$show(length xs)++" type-matching instances of "++show ident++" defined")pos   
 where
  finfo' :: Either ParseError [VFInstance]
  finfo' = case getVFContents stat ident of 
   Nothing          -> Left $newErrorMessage(Message$"function "++show ident++" is not defined")pos 
   Just(East())     -> Left $newErrorMessage(Message$"cannot call "++show ident++" because it is defined as a variable")pos
   Just(West info) -> Right $ info 

getCall4Left :: SourcePos -> NonEmpty (Value, Oper) -> UserState -> Either ParseError (ValueList, Oper)
getCall4Left pos (x:|xs) stat = do  
 opfixity <- getOpFixity pos stat op
 ops      <- getOpsFixities pos stat valuelist1
 mapM_ (`canBeLeftOf'` opfixity) ops
 return(valuelist1,op)
 where
  (top,mid,op) = shiftPair (x :| xs)
  valuelist1 :: ValueList
  valuelist1 = S.SepList(top,mid)
  canBeLeftOf' = canBeLeftOf pos

isValidCall3 :: SourcePos -> Oper -> ValueList -> UserState -> Either ParseError ()
isValidCall3 pos op valuelist2 stat = do
 opfixity <- getOpFixity pos stat op
 ops      <- getOpsFixities pos stat valuelist2
 mapM_ (`canBeRightOf'` opfixity) ops
 where 
  canBeRightOf' = canBeRightOf pos

  
-- FIXME :: SLOW
reverse'' :: (a,NonEmpty(b,a)) -> (a,NonEmpty(b,a))
reverse'' (a,(b,a2):|xs) = (q,ws `snoc2`(b,a))
 where S.SepList(q,ws) = S.reverse (S.SepList(a2,xs))

--breakBy' :: Oper -> NonEmptyValue -> (ValueList,ValueList)
-- FIXME: if Oper is not found, the list will be split by the last op. This can cause bugs, so you must be sure that Oper `elem` NonEmptyValue.
breakBy' :: (Eq o) => o -> (v,NonEmpty(o,v)) -> (S.SepList o v,S.SepList o v)
breakBy' o (v,list) = (S.SepList(v,a),S.SepList(b,c))
 where (a,b,c) = breakBy2 o list

breakBy2 :: (Eq o) => o -> NonEmpty(o,v) -> ([(o,v)],v,[(o,v)]) 
breakBy2 _ ((_ ,v2):|[])  = ([],v2,[]) -- FIXME
breakBy2 o ((o2,v2):|(ov3:ovs)) 
 | o == o2    = ([]       ,v2,ov3:ovs)
 | otherwise  = ((o2,v2):a,b ,c      )
 where 
  (a,b,c) = breakBy2 o (ov3:|ovs)

getOpContents2 :: SourcePos -> UserState -> Oper -> Either ParseError OpInfo
getOpContents2 pos s o = case getOpContents s o of
 Nothing   -> Left $ newErrorMessage(Message$"operator "++show o++" is not defined")pos
 Just info -> Right$ info

 
canBeRightOf :: SourcePos -> Fixity -> Fixity -> Either ParseError ()
canBeRightOf pos f2 f1
 | v2 > v1 = Right()
 | v2 < v1 = __mkmsg pos (getOpName f1) (getOpName f2)
 where v1 = getFixValue f1; v2 = getFixValue f2;
canBeRightOf pos (InfixL _ nm1) (InfixL _ nm2) = __mkmsg pos nm1 nm2
canBeRightOf pos (InfixL _ nm1) (InfixR _ nm2) = __mixed pos nm1 nm2
canBeRightOf pos (InfixR _ nm1) (InfixL _ nm2) = __mixed pos nm1 nm2
canBeRightOf _   (InfixR _ _  ) (InfixR _ _  ) = Right()

canBeLeftOf :: SourcePos -> Fixity -> Fixity -> Either ParseError ()
canBeLeftOf pos f2 f1
 | v2 > v1 = Right()
 | v2 < v1 = __mkmsg pos (getOpName f1) (getOpName f2)
 where v1 = getFixValue f1; v2 = getFixValue f2;
canBeLeftOf pos (InfixR _ nm1) (InfixR _ nm2) = __mkmsg pos nm1 nm2
canBeLeftOf pos (InfixR _ nm1) (InfixL _ nm2) = __mixed pos nm1 nm2
canBeLeftOf pos (InfixL _ nm1) (InfixR _ nm2) = __mixed pos nm1 nm2
canBeLeftOf _   (InfixL _ _  ) (InfixL _ _  ) = Right()

__mkmsg :: SourcePos -> Oper -> Oper -> Either ParseError a
__mkmsg pos nm1 nm2 = Left $ newErrorMessage(Message$"operator "++show (unOp nm2)++" has smaller fixity than its outer operator "++show (unOp nm1))pos
 
__mixed :: SourcePos -> Oper -> Oper -> Either ParseError a
__mixed pos nm1 nm2 = Left $ newErrorMessage(Message$"operator "++show (unOp nm1)++" and operator "++show (unOp nm2)++" has opposite fixity and thus cannot coexist")pos  
  
toIdentList :: TypeList -> [Ident]
toIdentList (S.SepList((_,t),xs)) = t:[x|(_,(_,x))<-xs]
 
getOpFixity :: SourcePos -> UserState -> Oper -> Either ParseError Fixity
getOpFixity pos stat op = fmap fst $ getOpContents2 pos stat op  
 
getOpsFixities :: SourcePos -> UserState -> ValueList -> Either ParseError [Fixity]
getOpsFixities pos stat valuelist = mapM (getOpFixity pos stat) $ S.toSeparatorList valuelist

getOpsFixities' :: SourcePos -> UserState -> NonEmptyValue -> Either ParseError (NonEmpty Fixity)
getOpsFixities' pos stat (_,(op,_):|ovs) = do
  fx  <- getOpFixity pos stat op
  fxs <- mapM (\(o,_) -> getOpFixity pos stat o) ovs
  return(fx:|fxs)

makeReplacerTable :: TypeList -> ValueList -> ReplTable
makeReplacerTable tlist vlist = M.fromList$zip(toIdentList tlist)(toList' vlist) 

makeReplacerTable2 :: (TypeList,TypeList) -> (ValueList,ValueList) -> ReplTable
makeReplacerTable2 (t1,t2)(v1,v2) = M.fromList$zip(toIdentList t1++toIdentList t2)(toList' v1++toList' v2)

isConsistent :: [Fixity] -> Bool
isConsistent xs = all isInfixL xs || all isInfixR xs

contradiction :: [Fixity] -> Maybe Fixity
contradiction [ ] = Nothing
contradiction [_] = Nothing
contradiction (InfixL _ _:xs) = listToMaybe $ filter isInfixR xs 
contradiction (InfixR _ _:xs) = listToMaybe $ filter isInfixL xs 