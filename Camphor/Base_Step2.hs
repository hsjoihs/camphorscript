{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
{- Functional macro expansion -}
module Camphor.Base_Step2
(step2
--,parser2'

) where


import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Control.Applicative hiding(many,(<|>))
import Camphor.Partial
import Camphor.Base_Step2_2
import Camphor.UserState
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import Text.Parsec 
import qualified Data.Map as M

step2 ::  FilePath -> Txt -> Either ParseError Txt
step2 file txt = do
 xs <- parse parser2'  (file ++ "-step2"  ) txt
 ys <- parse parser2_2 (file ++ "-step2-2") xs
 zs <- convert ys
 return zs
 

 
defaultStat :: UserState 
defaultStat = emptyState

convert :: Sents -> Either ParseError Txt
convert xs = convert2 defaultStat xs 

convert2 :: UserState -> Sents -> Either ParseError Txt

convert2 _    []                       = Right "" 
convert2 stat (Single(_,Comm comm):xs) = (("/*"++comm++"*/")++)     <$> convert2 stat     xs
convert2 stat (Single(_,Sp   sp  ):xs) = (sp++)                     <$> convert2 stat     xs
convert2 stat (Single(_,Scolon   ):xs) = (";"++)                    <$> convert2 stat     xs

convert2 stat (Single(pos,Char iden):xs) = do
 newStat <- newC pos iden stat 
 left <- convert2 newStat  xs
 return ("char " ++ iden ++";"++left)
 
convert2 stat (Single(pos,Del  iden):xs) = do
 newStat <- newD pos iden stat -- FIXME
 left <- convert2 newStat  xs
 return ("delete " ++iden++";"++left)

convert2 stat (Single(pos,Infl fixity op):xs) = do
 newStat <- newL pos fixity op stat 
 left <- convert2 newStat xs 
 return left
 
convert2 stat (Single(pos,Infr fixity op):xs) = do
 newStat <- newR pos fixity op stat 
 left <- convert2 newStat xs 
 return left
 
convert2 stat (Single(pos,Func2 op typelist1 typelist2 sent):xs) = do
 newStat <- newF2 pos op typelist1 typelist2 sent stat
 left <- convert2 newStat xs
 return left
 
convert2 stat (Single(pos,Func1 ident typelist sent):xs) = undefined
convert2 stat (Single(pos,Call1 ident valuelist):xs) = undefined

--- (val [op val])op(val [op val]);
convert2 stat (Single(pos,Call2 op valuelist1 valuelist2):xs) = do
 (result,newStat) <- newK2 pos op valuelist1 valuelist2 stat
 left <- convert2 newStat xs
 return(result ++ left)


convert2 stat (Single(pos,Call3 op  valuelist1 valuelist2):xs) = undefined
convert2 stat (Single(pos,Call4 list  valuelist):xs) = undefined
convert2 stat (Single(pos,Call5 valuelist):xs) = undefined
convert2 stat (Block ys:xs) = undefined


newC :: SourcePos -> Ident -> UserState -> Either ParseError UserState
newC pos ident stat 
 | stat `containsIdent` ident = Left $newErrorMessage(Message$"identifier "++show ident++" is already defined")pos
 | otherwise                  = Right$addIdent stat ident (Left())

newD :: SourcePos -> Ident -> UserState -> Either ParseError UserState
newD pos ident stat 
 | stat `containsIdent` ident = Right$removeIdent stat ident -- functions can also be deleted
 | otherwise                  = Left $newErrorMessage(Message$"identifier "++show ident++" is not defined")pos

newL :: SourcePos -> Fix -> Oper -> UserState -> Either ParseError UserState
newL pos fixity op stat = case findOpContents stat op of
 Just(fix,_) -> 
  if fix == InfixL fixity op 
  then Right stat 
  else Left$newErrorMessage(Message$"conflicting fixity definitions of operator "++show op)pos
 Nothing     -> Right$addOpFixity stat (InfixL fixity op)


newR :: SourcePos -> Fix -> Oper -> UserState -> Either ParseError UserState
newR pos fixity op stat = case findOpContents stat op of
 Just(fix,_) -> 
  if fix == InfixR fixity op 
  then Right stat 
  else Left$newErrorMessage(Message$"conflicting fixity definitions of operator "++show op)pos
 Nothing     -> Right$addOpFixity stat (InfixR fixity op)
 
-- Operator definition 
newF2 :: SourcePos -> Oper -> TypeList -> TypeList -> Sent -> UserState -> Either ParseError UserState
newF2 pos op typelist1 typelist2 sent stat@(UserState vflist oplist) = case findOpContents stat op of
 Nothing        -> Left $newErrorMessage(Message$"fixity of operator "++show op++" is not defined")pos
 Just(fix,list) -> Right$UserState vflist newOplist 
  where 
   newOplist = M.insert op (fix,newlist) oplist
   newlist = (typelist1,typelist2,sent):list -- FIXME : does not check the double definition

-- normalized operator call
newK2 :: SourcePos -> Oper -> ValueList -> ValueList -> UserState -> Either ParseError (Txt,UserState)   
newK2 pos op valuelist1 valuelist2 stat = do
 result <- replaceOpMacro pos op valuelist1 valuelist2 stat
 return(result,stat) -- stat is unchanged


--- macro-replacing function for operator
replaceOpMacro :: SourcePos -> Oper -> ValueList -> ValueList -> UserState -> Either ParseError Txt
replaceOpMacro pos op valuelist1 valuelist2 stat = undefined
 where
  opinfo = case findOpContents stat op of  -- Either ParseError [(TypeList,TypeList, Sent)]
   Nothing       -> Left $newErrorMessage(Message$"operator "++show op++" is not defined")pos 
   Just (_,info) -> Right info
  