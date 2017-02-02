{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base_Step2_3
(parser2_2
,parser2'
--,definition
)where

import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Global.Synonyms
import Text.Parsec 
import Control.Applicative hiding ((<|>),many)
import Camphor.Base_Step2_2
import Data.Functor.Identity
import Camphor.Partial

{-
definition :: Stream s Identity (SourcePos, Sent) => Parsec s u Ident
definition = parseWhen qqq
 where qqq(Single(_,Char t)) = Just t ; qqq _ = Nothing
 
parseWhen :: Stream s Identity (SourcePos, Sent) => ((SourcePos, Sent) -> Maybe a) -> Parsec s u a
parseWhen = token showSent2 xx
 where xx(Single(p,_))=p

showSent2 :: Sent -> String
showSent2 = undefined

-}