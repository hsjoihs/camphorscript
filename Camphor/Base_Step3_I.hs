{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind -fno-warn-unused-imports #-}
module Camphor.Base_Step3_I
(step3_I
)where
import Camphor.Global
import Camphor.Global
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)

step3_I :: FilePath -> Txt -> Either s Txt
step3_I _ = Right