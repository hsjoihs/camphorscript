{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall  #-}
module Camphor.Base.Base_Step2.Call5Result
(getCall5Result
) where 
import qualified Camphor.SepList as S
import Camphor.Base.Base_Step2.Type
import Camphor.SafePrelude
import Camphor.Base.Base_Step2.Auxilary
import Camphor.Base.Base_Step2.UserState
import Camphor.Base.Base_Step2.Auxilary2
import Camphor.Base.Base_Step2.ErrList
import Camphor.Global.Synonyms
import Camphor.NonEmpty
import Text.Parsec 

contradiction :: [Fixity] -> Maybe Fixity
contradiction [ ] = Nothing
contradiction [_] = Nothing
contradiction (InfixL _ _:xs) = listToMaybe $ filter isInfixR xs -- InfixR INTENTIONAL
contradiction (InfixR _ _:xs) = listToMaybe $ filter isInfixL xs -- InfixL INTENTIONAL

minimumsBy :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
minimumsBy cmp_ (x_ :| xs_) = go2 cmp_ xs_ (x_:|[])
 where 
  go2 _ [] acc = acc
  go2 cmp (x:xs) acc = go cmp (x:|xs) acc
  go cmp (x :| xs) acc@(y :| _) = case cmp x y of
   GT -> go2 cmp xs acc --- x > y
   EQ -> go2 cmp xs (x `cons` acc) -- x == y
   LT -> go2 cmp xs (x :| []) -- x < y

type Cmp a = a -> a -> Ordering
type MCmp a = a -> a -> Either ParseError Ordering
   
minimumsBy2 :: Cmp Fixity -> NonEmpty Fixity -> (Fixity -> Fixity -> e) -> Either e Fixity
minimumsBy2 cmp xs e = do
 let (k:|ks) = minimumsBy cmp xs -- search for minimums
 case contradiction (k:ks) of
  Nothing -> return k      -- got the minimum
  Just k2 -> Left $ e k k2 -- if inconsistent(mixture of L and R) error 

-- break by the smallest o (leftmost)
breakByFirstMinimum :: MCmp Oper -> NonEmptyValue -> Either ParseError(Oper,ValueList,ValueList)
breakByFirstMinimum cmp (v,list) = do 
 (minOp,left,right) <- bBFM cmp list
 return(minOp,S.SepList v left,right)

bBFM :: MCmp Oper -> NonEmpty(Oper,Value) -> Either ParseError (Oper,[(Oper,Value)],ValueList)
bBFM _   ((o,v) :| []) = return(o,[],return v)
bBFM cmp ((o,v) :| (ov2:ovs)) = do
 (minOp',left',right'@(S.SepList v2 rest)) <- bBFM cmp (ov2:|ovs)
 res <- cmp o minOp'
 case res of
  LT -> return(o     ,[]         ,S.SepList v(left' ++ [(minOp',v2)] ++ rest))
  _  -> return(minOp',(o,v):left',right')

  
-- break by the smallest Oper (rightmost)
breakByLastMinimum :: MCmp Oper -> NonEmptyValue -> Either ParseError (Oper,ValueList,ValueList)
breakByLastMinimum cmp nEvaluelist = do
 let list2 = reverse'' nEvaluelist
 (oper,vlist2',vlist1') <- breakByFirstMinimum cmp list2
 let vlist1 = S.reverse vlist1'; vlist2 = S.reverse vlist2'
 return(oper,vlist1,vlist2)

cmpOper :: SourcePos -> UserState -> Oper -> Oper -> Either ParseError Ordering  
cmpOper pos stat o1 o2 = do
 f1 <- getOpFixity pos stat o1
 f2 <- getOpFixity pos stat o2
 return $ comparing(getFixValue) f1 f2

  
getCall5Result :: SourcePos -> NonEmptyValue -> UserState -> Either ParseError (Oper,ValueList,ValueList)
getCall5Result pos nEvaluelist stat = do
 fixes <- getOpsFixities' pos stat nEvaluelist
 minOp <- minimumsBy2 (comparing getFixValue) fixes (\k k2 -> toPE pos$ Step2 <!> Type <!> WrongCall <!> Infixconflict k k2) -- borrowed from GHC 
 case minOp of
  InfixL _ _ -> breakByFirstMinimum (cmpOper pos stat) nEvaluelist 
  InfixR _ _ -> breakByLastMinimum (cmpOper pos stat) nEvaluelist 
