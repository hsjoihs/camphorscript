{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  -fno-warn-unused-imports #-}
{- (in|de)crement optimization -}
module Camphor.Base_Step6
(step6
) where

import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Global.Synonyms
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)

step6 :: FilePath -> Txt -> Either s Txt
step6 _ = Right
