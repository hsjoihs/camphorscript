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
{- 
data SimpleSent = 
 Func1 Ident TypeList1 Sent | Func2 Oper TypeList1 TypeList1 Sent | Call1 Ident ValueList |
 Call2 Oper ValueList ValueList | Call3 Oper ValueList ValueList | Call4 [(Value,Oper)] ValueList | Call5 ValueList
-}
 

newC :: SourcePos -> Ident -> UserState -> Either ParseError UserState
newC pos ident stat 
 | stat `containsIdent` ident = Left $newErrorMessage(Message$"identifier "++show ident++" is already defined")pos
 | otherwise                  = Right$addIdent stat ident (Left())

newD :: SourcePos -> Ident -> UserState -> Either ParseError UserState
newD pos ident stat 
 | stat `containsIdent` ident = Right$removeIdent stat ident -- functions can also be deleted
 | otherwise                  = Left $newErrorMessage(Message$"identifier "++show ident++" is not defined")pos

newL :: SourcePos -> Integer -> Oper -> UserState -> Either ParseError UserState
newL pos fixity op stat = case findOpContents stat op of
 Just(fix,_) -> 
  if fix == InfixL fixity op 
  then Right stat 
  else Left$newErrorMessage(Message$"conflicting fixity definitions of operator "++show op)pos
 Nothing     -> Right$addOpFixity stat (InfixL fixity op)


newR :: SourcePos -> Integer -> Oper -> UserState -> Either ParseError UserState
newR pos fixity op stat = case findOpContents stat op of
 Just(fix,_) -> 
  if fix == InfixR fixity op 
  then Right stat 
  else Left$newErrorMessage(Message$"conflicting fixity definitions of operator "++show op)pos
 Nothing     -> Right$addOpFixity stat (InfixR fixity op)