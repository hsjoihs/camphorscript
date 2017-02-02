{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts , NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind -fno-warn-unused-imports #-}
module Camphor.Base.Base_Step3_I
(step3_I
)where
import Camphor.SafePrelude
import Camphor.Global.Parsers
import Camphor.Global.Synonyms
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)

step3_I :: FilePath -> Txt -> Either s Txt
step3_I _ = Right