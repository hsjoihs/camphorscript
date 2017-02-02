{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base.Base_Step2.Auxilary
(ReplTable,makeReplacerTable,makeReplacerTable2,makeReplacerTable3
,getOpFixity{- ,getOpsFixities -},getOpsFixities'
{- ,canBeRightOf,canBeLeftOf -}
-- ,getOpContents2
-- ,isConsistent
-- ,NonEmptyValue

,reverse''

,isValidCall3
,getCall4Left
,getInstanceOfCall1
,getInstanceOfCall2
,getInstanceOfSC

,getLastPos
-- ,toSents
,err
,toState,fromState,changeState

) where
import Camphor.SafePrelude
import qualified Camphor.SepList as S
import Camphor.Base.Base_Step2.Type
import Camphor.Base.Base_Step2.UserState
import Camphor.Base.Base_Step2.Auxilary2
import Camphor.Global.Synonyms
import Text.Parsec 
import Camphor.NonEmpty
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
import Camphor.Base.Base_Step2.ErrList

getLastPos :: Sent -> SourcePos
getLastPos (Single pos _) = pos
getLastPos (Block  p [] ) = p
getLastPos (Block  _ (x:xs)) = getLastPos $ last' (x :| xs)

getOpContents2 :: SourcePos -> UserState -> Oper -> Either ParseError OpInfo
getOpContents2 pos s o = case getOpContents s o of
 Nothing     -> Left $toPE pos $ Step2 <!> Type <!> WrongCall <!> Fixnotdefined_2 <!> Operat_3 o
 Just info   -> Right$ info


getInstanceOfCall2 :: SourcePos -> Oper -> ValueList -> ValueList -> UserState -> Either ParseError OpInstance
getInstanceOfCall2 pos op valuelist1 valuelist2 stat = do
 (_,opinfo) <- getOpContents2 pos stat op
 when(null opinfo) $ Left $toPE pos $ Step2 <!> Type <!> WrongCall <!> Notdefined <!> Operat op 
 let matchingOpInstance = [ a | a@(typelist1,typelist2,_) <- opinfo, valuelist1 `matches` typelist1, valuelist2 `matches` typelist2 ] 
 case matchingOpInstance of 
  []        -> Left $toPE pos $ Step2 <!> Type <!> WrongCall <!> Notypematch <!> Operat op
  [instnce] -> return instnce
  xs        -> Left $toPE pos $ Step2 <!> Type <!> WrongCall <!> Manytypematches (length xs) <!> Operat op 

getInstanceOfCall1 :: SourcePos -> Ident2 -> ValueList -> UserState -> Either ParseError VFInstance
getInstanceOfCall1 pos ident valuelist stat = case getVFContents stat ident of -- checks if `ident' is a function; if so, look for all the instances.
 Nothing              -> Left $toPE pos $ Step2 <!> Type <!> WrongCall <!> Notdefined <!> Functi ident
 Just Variable        -> Left $toPE pos $ Step2 <!> Type <!> WrongCall <!> Definedasvar <!> Functi_2 ident
 Just(FunSyn finfo _) -> do 
  let matchingFuncInstance = [ a | a@(typelist,_) <- finfo, valuelist `matches` typelist ]
  case matchingFuncInstance of
   []        -> Left $toPE pos $ Step2 <!> Type <!> WrongCall <!> Notypematch <!> Functi ident 
   [instnce] -> return instnce
   xs        -> Left $toPE pos $ Step2 <!> Type <!> WrongCall <!> Manytypematches (length xs) <!> Functi ident   

getInstanceOfSC :: SourcePos -> Ident2 -> Between TailValueList ValueList -> UserState -> Either ParseError (SyntaxInstance,ReplTable)  
getInstanceOfSC pos ident vl_vvl stat = case getVFContents stat ident of -- checks if `ident' is a syntax; if so, look for all the instances.
 Nothing              -> Left $toPE pos $ Step2 <!> Type <!> WrongCall <!> Notdefined <!> Synt ident
 Just Variable        -> Left $toPE pos $ Step2 <!> Type <!> WrongCall <!> Definedasvar <!> Synt_2 ident
 Just(FunSyn _ sinfo) -> case getMatchingSyntaxInstRepls sinfo vl_vvl of 
  []        -> Left $toPE pos $ Step2 <!> Type <!> WrongCall <!> Notypematch <!> Synt ident 
  xs@(_:_:_)-> Left $toPE pos $ Step2 <!> Type <!> WrongCall <!> Manytypematches (length xs) <!> Synt ident    
  [instrepl]-> return instrepl

getMatchingSyntaxInstRepls :: [SyntaxInstance] -> Between TailValueList ValueList -> [(SyntaxInstance,ReplTable)] 
getMatchingSyntaxInstRepls sinfo vl_vvl = do
 a@(tl_ttl,_) <- sinfo
 case (vl_vvl,tl_ttl) of
  (West _  ,East _  ) -> [] -- failure
  (East _  ,West _  ) -> [] -- failure
  (West vl ,West tl ) -> guard (vl `matches` tl) >> return (a,makeReplacerTable tl vl)
  (East tvl,East ttl) -> guard (tvl `matches2` ttl) >> return (a,makeReplacerTable3 ttl tvl)



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


 
canBeRightOf :: SourcePos -> Fixity -> Fixity -> Either ParseError ()
canBeRightOf pos f2 f1
 | v2 > v1 = Right()
 | v2 < v1 = __mkmsg pos (getOpName f1) (getOpName f2)
 where v1 = getFixValue f1; v2 = getFixValue f2;
canBeRightOf pos     (InfixL _ nm1)     (InfixL _ nm2) = __mkmsg pos nm1 nm2
canBeRightOf pos nm1@(InfixL _ _  ) nm2@(InfixR _ _  ) = __mixed pos nm1 nm2
canBeRightOf pos nm1@(InfixR _ _  ) nm2@(InfixL _ _  ) = __mixed pos nm1 nm2
canBeRightOf _       (InfixR _ _  )     (InfixR _ _  ) = Right()

canBeLeftOf :: SourcePos -> Fixity -> Fixity -> Either ParseError ()
canBeLeftOf pos f2 f1
 | v2 > v1 = Right()
 | v2 < v1 = __mkmsg pos (getOpName f1) (getOpName f2)
 where v1 = getFixValue f1; v2 = getFixValue f2;
canBeLeftOf pos     (InfixR _ nm1)     (InfixR _ nm2) = __mkmsg pos nm1 nm2
canBeLeftOf pos nm1@(InfixR _ _  ) nm2@(InfixL _ _  ) = __mixed pos nm1 nm2
canBeLeftOf pos nm1@(InfixL _ _  ) nm2@(InfixR _ _  ) = __mixed pos nm1 nm2
canBeLeftOf _       (InfixL _ _  )     (InfixL _ _  ) = Right()

__mkmsg :: SourcePos -> Oper -> Oper -> Either ParseError a
__mkmsg pos nm1 nm2 = Left $ toPE pos $ Step2 <!> Type <!> WrongCall <!> Smaller nm2  <!> Operat_3 nm1
 
__mixed :: SourcePos -> Fixity -> Fixity -> Either ParseError a
__mixed pos nm1 nm2 = Left $ toPE pos $ Step2 <!> Type <!> WrongCall <!> Infixconflict nm1 nm2 
  
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

makeReplacerTable3 :: TailTypeList -> TailValueList -> ReplTable
makeReplacerTable3 ttl tvl = M.fromList$zip(map snd $ toList' ttl)(toList' tvl)

-- isConsistent :: [Fixity] -> Bool
-- isConsistent xs = all isInfixL xs || all isInfixR xs


-- toSents :: Extra -> [SimpleSent] -> Sents
-- toSents  = map . Single

err :: MonadTrans t => a1 -> t (Either a1) a
err = lift . Left 

toState :: (Monad m) => ReaderT r m a -> StateT r m a
toState (ReaderT f) = StateT $ \s -> do
 a <- f s
 return (a,s)
 
fromState :: (Functor m,Monad m) => StateT r m a -> ReaderT r m a
fromState (StateT f) = ReaderT $ (fst <$>) . f
 
changeState :: (Monad m) => (r -> s) -> (s -> r) -> StateT r m a -> StateT s m a
changeState for back (StateT f) = StateT $ \s -> do
 (a,r) <- f (back s)
 return (a,for r)