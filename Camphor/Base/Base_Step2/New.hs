{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall  #-}
{- Functional macro expansion -}
module Camphor.Base.Base_Step2.New
(newC,newD,newL,newR,newF1,newF2,getCall5Result
) where 

import qualified Camphor.SepList as S
import Camphor.Base.Base_Step2.Type
import Camphor.SafePrelude
import Data.Ord(comparing)
import Camphor.Base.Base_Step2.Auxilary
import Camphor.Base.Base_Step2.UserState
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import Camphor.NonEmpty
import Text.Parsec 

newC :: SourcePos -> Ident -> UserState -> Either ParseError UserState
newC pos ident stat 
 | stat `containsIdent` ident = Left $newErrorMessage(Message$"identifier "++show ident++" is already defined")pos
 | otherwise                  = Right$addIdent stat ident (East())
 
newD :: SourcePos -> Ident -> UserState -> Either ParseError UserState
newD pos ident stat 
 | stat `containsIdent` ident = Right$removeIdent stat ident -- functions can also be deleted
 | otherwise                  = Left $newErrorMessage(Message$"identifier "++show ident++" is not defined")pos

newL :: SourcePos -> Fix -> Oper -> UserState -> Either ParseError UserState
newL pos fixity op stat = case getOpContents stat op of
 Nothing     -> Right$addOpFixity stat (InfixL fixity op)
 Just(fix,_) 
  | fix == InfixL fixity op -> Right stat 
  | otherwise               -> Left$newErrorMessage(Message$"conflicting fixity definitions of operator "++show op)pos
 
newR :: SourcePos -> Fix -> Oper -> UserState -> Either ParseError UserState
newR pos fixity op stat = case getOpContents stat op of
 Nothing     -> Right$addOpFixity stat (InfixR fixity op)
 Just(fix,_) 
  | fix == InfixR fixity op -> Right stat
  | otherwise               -> Left$newErrorMessage(Message$"conflicting fixity definitions of operator "++show op)pos

-- Function definition
newF1 :: SourcePos -> Ident -> TypeList -> Maybe Sent -> UserState -> Either ParseError UserState
newF1 pos name typelist sent stat
 | typelistIdentConflict typelist = Left $ newErrorMessage(Message$"overlapping parameters of function "++show name)pos
 | otherwise = case getVFContents stat name of
  Just(East ())  -> Left $newErrorMessage(Message$"cannot define function "++show name++" because it is already defined as a variable")pos
  Nothing        -> Right$addIdent stat name (West[(typelist,sent)])
  Just(West xs) 
   | any (\(tlist,_) -> typelist `overlaps` tlist) xs   -> Left $ newErrorMessage(Message$"type-overlapping definition of function"++show name)pos
   | otherwise                                          -> return $ addIdent stat name (West$(typelist,sent):xs)
 
-- Operator definition 
newF2 :: SourcePos -> Oper -> TypeList -> TypeList -> Maybe Sent -> UserState -> Either ParseError UserState
newF2 pos op typelist1 typelist2 sent stat = 
 addOpContents stat op (typelist1,typelist2,sent) $ 
  (newErrorMessage(Message$"fixity of operator "++show op++" is not defined")pos,
  newErrorMessage(Message$"type-overlapping definition of operator "++show op)pos,
  newErrorMessage(Message$"overlapping parameters of operator "++show op)pos)
   
getCall5Result :: SourcePos -> NonEmptyValue -> UserState -> Either ParseError (Oper,ValueList,ValueList)
getCall5Result pos nEvaluelist stat = do
 fixes <- getOpsFixities' pos stat nEvaluelist
 let minOps = minimumsBy (comparing getFixValue) fixes
 case minOps of 
  k               :| [] -> newK5_2 nEvaluelist (getOpName k)
  k@(InfixL _ op) :| ks -> case contradiction(k:ks) of
   Nothing -> newK5_2 nEvaluelist op
   Just k2 -> Left $newErrorMessage(Message$"cannot mix "++show' k++" and "++show' k2++" in the same infix expression")pos -- message borrowed from GHC
  k@(InfixR _ op) :| ks -> case contradiction(k:ks) of
   Nothing -> newK5_3 nEvaluelist op
   Just k2 -> Left $newErrorMessage(Message$"cannot mix "++show' k++" and "++show' k2++" in the same infix expression")pos -- message borrowed from GHC
 
newK5_2 :: NonEmptyValue -> Oper -> Either ParseError (Oper,ValueList,ValueList)
newK5_2 nEvaluelist oper = return(oper,vlist1,vlist2)
 where (vlist1,vlist2) = breakBy' oper nEvaluelist 
 
newK5_3 :: NonEmptyValue -> Oper -> Either ParseError (Oper,ValueList,ValueList)
newK5_3 nEvaluelist oper = return(oper,vlist1,vlist2)
 where 
  (vlist2',vlist1') = breakBy' oper (reverse'' nEvaluelist)
  (vlist1,vlist2) = (S.reverse vlist1',S.reverse vlist2')