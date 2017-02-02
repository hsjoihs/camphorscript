{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  -fno-warn-unused-imports #-}
{- (in|de)crement optimization -}
module Camphor.Base_Step6
(
step6
) where


import Camphor.Global
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)

step6 :: FilePath -> Txt -> Either s Txt
step6 _ = Right
