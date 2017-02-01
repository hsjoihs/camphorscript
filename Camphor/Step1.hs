{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  -fno-warn-missing-signatures #-}
module Camphor.Step1
(

) where


import Text.Parsec
import Control.Applicative hiding ((<|>),many)
import Data.List(genericTake)
import Control.Monad((>=>))

{- C macro  -}

lines=many line
 where line=do{xs<-many(noneOf "\n");return xs;}
{-separation by line -} 

