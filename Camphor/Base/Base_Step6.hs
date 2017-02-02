{-# LANGUAGE FlexibleContexts , NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  -fno-warn-unused-imports #-}
{- (in|de)crement optimization -}
module Camphor.Base.Base_Step6
(step6
) where

import Camphor.SafePrelude 
import Camphor.Global.Synonyms
import Text.Parsec hiding(token)

step6 :: FilePath -> Txt -> Either s Txt
step6 _ = Right
