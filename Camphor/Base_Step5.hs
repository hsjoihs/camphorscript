{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  -fno-warn-unused-imports #-}
{- memory allocation -}
module Camphor.Base_Step5
(step5
) where

import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Global.Synonyms
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)

step5 :: FilePath -> Txt -> Either s Txt
step5 _ = Right
