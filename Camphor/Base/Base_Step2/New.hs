{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall  #-}
{- Functional macro expansion -}
module Camphor.Base.Base_Step2.New
(newC,newD,newL,newR,newF1,newF2,newS1,newS2
) where 

import Camphor.Base.Base_Step2.Type
import Camphor.SafePrelude
import Camphor.Base.Base_Step2.UserState
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import Text.Parsec 

newC :: SourcePos -> Ident2 -> UserState -> Either ParseError UserState
newC pos ident stat 
 | stat `containsIdent` ident = Left $newErrorMessage(Message$"identifier "++showIdent ident++" is already defined")pos
 | otherwise                  = Right$addVar stat ident
 
newD :: SourcePos -> Ident2 -> UserState -> Either ParseError UserState
newD pos ident stat 
 | stat `containsIdent` ident = Right$removeIdent stat ident -- functions can also be deleted
 | otherwise                  = Left $newErrorMessage(Message$"identifier "++showIdent ident++" is not defined")pos

newL :: SourcePos -> Fix -> Oper -> UserState -> Either ParseError UserState
newL pos fixity op stat = case getOpContents stat op of
 Nothing     -> Right$addOpFixity stat (InfixL fixity op)
 Just(fix,_) 
  | fix == InfixL fixity op -> Right stat 
  | otherwise               -> Left$newErrorMessage(Message$"conflicting fixity definitions of operator "++showStr (unOp op))pos
 
newR :: SourcePos -> Fix -> Oper -> UserState -> Either ParseError UserState
newR pos fixity op stat = case getOpContents stat op of
 Nothing     -> Right$addOpFixity stat (InfixR fixity op)
 Just(fix,_) 
  | fix == InfixR fixity op -> Right stat
  | otherwise               -> Left$newErrorMessage(Message$"conflicting fixity definitions of operator "++showStr (unOp op))pos

-- Function definition
newF1 :: SourcePos -> Ident2 -> TypeList -> Maybe Sent -> UserState -> Either ParseError UserState
newF1 pos name typelist sent stat
 | typelistIdentConflict typelist = Left $ newErrorMessage(Message$"overlapping parameters of function "++showIdent name)pos
 | otherwise = case getVFContents stat name of
  Just Variable  -> Left $newErrorMessage(Message$"cannot define function " ++ showIdent name ++ " because it is already defined as a variable")pos
  Nothing        -> Right$addFunSyn stat name [(typelist,sent)] []
  Just(FunSyn xs ys) 
   | any (\(tlist,_) -> typelist `overlaps` tlist) xs   -> Left $ newErrorMessage(Message$"type-overlapping definition of function"++showIdent name)pos
   | otherwise                                          -> return $ addFunSyn stat name ((typelist,sent):xs) ys
 
-- Operator definition 
newF2 :: SourcePos -> Oper -> TypeList -> TypeList -> Maybe Sent -> UserState -> Either ParseError UserState
newF2 pos op typelist1 typelist2 sent stat = 
 addOpContents stat op (typelist1,typelist2,fmap toSent2 sent) $ 
  (newErrorMessage(Message$"fixity of operator "++showStr (unOp op)++" is not defined")pos,
  newErrorMessage(Message$"type-overlapping definition of operator "++showStr (unOp op))pos,
  newErrorMessage(Message$"overlapping parameters of operator "++showStr (unOp op))pos)

-- Syntax1 definition  
newS1 :: SourcePos -> Ident2 -> TypeList -> Ident2 -> Sent -> UserState -> Either ParseError UserState 
newS1 pos name tl arg sent stat 
 | typelistIdentConflict tl = Left $ newErrorMessage(Message$"overlapping parameters of syntax "++showIdent name)pos
 | otherwise = case getVFContents stat name of
  Just Variable  -> Left $newErrorMessage(Message$"cannot define syntax " ++ showIdent name ++ " because it is already defined as a variable")pos
  Nothing        -> Right$addFunSyn stat name [] [(West tl,arg,sent)]
  Just(FunSyn xs ys) 
   | any (\(tlist,_,_) -> tl `overlaps2` tlist) ys -> Left $ newErrorMessage(Message$"type-overlapping definition of syntax"++showIdent name)pos
   | otherwise                                     -> return $ addFunSyn stat name xs ((West tl,arg,sent):ys)
   
overlaps2 :: TypeList -> Between TailTypeList TypeList -> Bool
overlaps2 tl (West tl2) = tl `overlaps` tl2
overlaps2 _  _          = False

overlaps3 :: TailTypeList -> Between TailTypeList TypeList -> Bool
overlaps3 ttl (East ttl2) = ttl `overlaps'` ttl2
overlaps3 _  _            = False

newS2 :: SourcePos -> Ident2 -> TailTypeList -> Ident2 -> Sent -> UserState -> Either ParseError UserState 
newS2 pos name ttl arg sent stat
 | tailTypelistIdentConflict ttl = Left $ newErrorMessage(Message$"overlapping parameters of syntax "++showIdent name)pos
 | otherwise = case getVFContents stat name of
  Just Variable  -> Left $newErrorMessage(Message$"cannot define syntax " ++ showIdent name ++ " because it is already defined as a variable")pos
  Nothing        -> Right$addFunSyn stat name [] [(East ttl,arg,sent)]
  Just(FunSyn xs ys) 
   | any (\(tlist,_,_) -> ttl `overlaps3` tlist) ys -> Left $ newErrorMessage(Message$"type-overlapping definition of syntax"++showIdent name)pos
   | otherwise                                      -> return $ addFunSyn stat name xs ((East ttl,arg,sent):ys)