{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base_Step2_3
(parser2_2
,parser2'
)where

import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Global.Synonyms
import Text.Parsec 
import Control.Applicative hiding ((<|>),many)
import Camphor.Base_Step2_2
import Data.Functor.Identity


