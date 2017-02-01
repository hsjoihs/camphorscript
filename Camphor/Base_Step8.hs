{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
module Camphor.Base_Step8
(step8

,parser8
,convert8
) where

import Camphor.Global
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)
import Data.List(genericTake)
import Data.Functor.Identity


step8::Stream s Identity Char=>s->Either ParseError String
step8 str=convert8 <$> (parse parser8 "step8" str) {-removes unnecessary letters-}

parser8::Stream s m Char=>ParsecT s u m [String]
parser8 = many char'
 where char' = do{x<-oneOf "+-<>[],.";return [x]} <|> (noneOf "+-<>[],.">>return "")
 

convert8::[String]->String
convert8 = concat