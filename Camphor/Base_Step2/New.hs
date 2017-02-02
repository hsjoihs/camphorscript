{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
{- Functional macro expansion -}
module Camphor.Base_Step2.New
(newC,newD,newL,newR,newF1,newF2
) where 

import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Base_Step2.Base_Step2_2
import Camphor.Base_Step2.UserState
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import Text.Parsec 
import qualified Data.Map as M

newC :: SourcePos -> Ident -> UserState -> Either ParseError UserState
newC pos ident stat 
 | stat `containsIdent` ident = Left $newErrorMessage(Message$"identifier "++show ident++" is already defined")pos
 | otherwise                  = Right$addIdent stat ident (Left())
 
newD :: SourcePos -> Ident -> UserState -> Either ParseError UserState
newD pos ident stat 
 | stat `containsIdent` ident = Right$removeIdent stat ident -- functions can also be deleted
 | otherwise                  = Left $newErrorMessage(Message$"identifier "++show ident++" is not defined")pos

newL :: SourcePos -> Fix -> Oper -> UserState -> Either ParseError UserState
newL pos fixity op stat = case getOpContents stat op of
 Just(fix,_) -> 
  if fix == InfixL fixity op 
  then Right stat 
  else Left$newErrorMessage(Message$"conflicting fixity definitions of operator "++show op)pos
 Nothing     -> Right$addOpFixity stat (InfixL fixity op)
 
newR :: SourcePos -> Fix -> Oper -> UserState -> Either ParseError UserState
newR pos fixity op stat = case getOpContents stat op of
 Just(fix,_) -> 
  if fix == InfixR fixity op 
  then Right stat 
  else Left$newErrorMessage(Message$"conflicting fixity definitions of operator "++show op)pos
 Nothing     -> Right$addOpFixity stat (InfixR fixity op)

-- Function definition
newF1 :: SourcePos -> Ident -> TypeList -> Sent -> UserState -> Either ParseError UserState
newF1 pos name typelist sent stat = case getVFContents stat name of
 Just(Left ())  -> Left $newErrorMessage(Message$"cannot define function"++show name++" because it is already defined as a variable")pos
 Nothing        -> Right$addIdent stat name (Right[(typelist,sent)])
 Just(Right xs) -> Right$addIdent stat name (Right$(typelist,sent):xs)
 
-- Operator definition 
newF2 :: SourcePos -> Oper -> TypeList -> TypeList -> Sent -> UserState -> Either ParseError UserState
newF2 pos op typelist1 typelist2 sent stat@(UserState vflist oplist) = case getOpContents stat op of
 Nothing        -> Left $newErrorMessage(Message$"fixity of operator "++show op++" is not defined")pos
 Just(fix,list) -> Right$UserState vflist newOplist 
  where 
   newOplist = M.insert op (fix,newlist) oplist
   newlist = (typelist1,typelist2,sent):list -- FIXME : does not check the double definition