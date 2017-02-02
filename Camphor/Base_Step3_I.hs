{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind -fno-warn-unused-imports #-}
module Camphor.Base_Step3_I
(step3_I
)where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Global.Parsers
import Camphor.Global.Synonyms
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)

step3_I :: FilePath -> Txt -> Either s Txt
step3_I _ = Right