{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base.Base_Step2.Auxilary
(getOpContents2
,canBeRightOf
,canBeLeftOf
,getOpFixity,getOpsFixities,getOpsFixities'
,makeReplacerTable,makeReplacerTable2,ReplTable
,isConsistent
,NonEmptyValue
,reverse''
,isValidCall3,getCall4Left,getInstanceOfCall1,getInstanceOfCall2
,getLastPos,toSents,err,toState,fromState
) where
import Camphor.SafePrelude
import qualified Camphor.SepList as S
import Camphor.Base.Base_Step2.Type
import Camphor.Base.Base_Step2.UserState
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import Text.Parsec 
import Camphor.NonEmpty
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader

getLastPos :: Sent -> SourcePos
getLastPos (Single pos _) = pos
getLastPos (Block  p [] ) = p
getLastPos (Block  _ (x:xs)) = getLastPos $ last' (x :| xs)

getInstanceOfCall2 :: SourcePos -> Oper -> ValueList -> ValueList -> UserState -> Either ParseError OpInstance
getInstanceOfCall2 pos op valuelist1 valuelist2 stat = do
 (_,opinfo) <- getOpContents2 pos stat op
 let matchingOpInstance = [ a | a@(typelist1,typelist2,_) <- opinfo, valuelist1 `matches` typelist1, valuelist2 `matches` typelist2 ] 
 case matchingOpInstance of 
  []        -> Left $newErrorMessage(Message$"no type-matching instance of "++showStr (unOp op)++" defined")pos 
  [instnce] -> return instnce
  xs        -> Left $newErrorMessage(Message$showNum(length xs)++" type-matching instances of "++showStr (unOp op)++" defined")pos 

getInstanceOfCall1 :: SourcePos -> Ident2 -> ValueList -> UserState -> Either ParseError VFInstance
getInstanceOfCall1 pos ident valuelist stat = do
 finfo <- finfo' -- checks if `ident' is a function; if so, look for all the instances.
 let matchingFuncInstance = [ a | a@(typelist,_) <- finfo, valuelist `matches` typelist ]
 case matchingFuncInstance of
  []        -> Left $newErrorMessage(Message$"no type-matching instance of "++showIdent ident++" defined")pos  
  [instnce] -> return instnce
  xs        -> Left $newErrorMessage(Message$showNum(length xs)++" type-matching instances of "++showIdent ident++" defined")pos   
 where
  finfo' :: Either ParseError [VFInstance]
  finfo' = case getVFContents stat ident of 
   Nothing          -> Left $newErrorMessage(Message$"function "++showIdent ident++" is not defined")pos 
   Just(East())     -> Left $newErrorMessage(Message$"cannot call "++showIdent ident++" because it is defined as a variable")pos
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
  valuelist1 = S.SepList top mid
  canBeLeftOf' = canBeLeftOf pos

isValidCall3 :: SourcePos -> Oper -> ValueList -> UserState -> Either ParseError ()
isValidCall3 pos op valuelist2 stat = do
 opfixity <- getOpFixity pos stat op
 ops      <- getOpsFixities pos stat valuelist2
 mapM_ (`canBeRightOf'` opfixity) ops
 where 
  canBeRightOf' = canBeRightOf pos

  
reverse'' :: (a,NonEmpty(b,a)) -> (a,NonEmpty(b,a))
reverse'' (a,(b,a2):|xs) = (q,ws `snoc2`(b,a))
 where S.SepList q ws = S.reverse (S.SepList a2 xs)

getOpContents2 :: SourcePos -> UserState -> Oper -> Either ParseError OpInfo
getOpContents2 pos s o = case getOpContents s o of
 Nothing   -> Left $ newErrorMessage(Message$"operator "++showStr (unOp o)++" is not defined")pos
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
__mkmsg pos nm1 nm2 = Left $ newErrorMessage(Message$"operator "++showStr (unOp nm2)++" has smaller fixity than its outer operator "++showStr (unOp nm1))pos
 
__mixed :: SourcePos -> Oper -> Oper -> Either ParseError a
__mixed pos nm1 nm2 = Left $ newErrorMessage(Message$"operator "++showStr (unOp nm1)++" and operator "++showStr (unOp nm2)++" has opposite fixity and thus cannot coexist")pos  
  
toIdentList :: TypeList -> [Ident2]
toIdentList (S.SepList (_,t) xs) = t:[x|(_,(_,x))<-xs]
 
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


toSents :: Extra -> [SimpleSent] -> Sents
toSents  = map . Single

err :: MonadTrans t => a1 -> t (Either a1) a
err = lift . Left 

toState :: (Monad m) => ReaderT r m a -> StateT r m a
toState (ReaderT f) = StateT $ \s -> do
 a <- f s
 return (a,s)
 
fromState :: (Monad m) => StateT r m a -> ReaderT r m a
fromState (StateT f) = ReaderT $ \s -> do
 (a,_) <- f s
 return a