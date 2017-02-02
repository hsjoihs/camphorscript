{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind   -fno-warn-unused-imports #-}
{- Functional macro expansion -}
module Camphor.Base_Step2
(

) where

import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Global
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)


