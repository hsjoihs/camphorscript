{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  -fno-warn-missing-signatures -fno-warn-unused-imports #-}
{- memory allocation -}
module Camphor.Base_Step5
(
step5
) where


import Camphor.Global
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)

step5 = Right
