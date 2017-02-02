{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts , NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  -fno-warn-unused-imports #-}
{- memory allocation -}
module Camphor.Base.Base_Step5
(step5
) where

import Camphor.SafePrelude 
import Camphor.Global.Synonyms
import Text.Parsec hiding(token)

step5 :: FilePath -> Txt -> Either s Txt
step5 _ = Right