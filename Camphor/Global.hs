{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  -fno-warn-missing-signatures -fno-warn-unused-imports #-}
module Global
(identifier
,identifier'
,nbsp
,nbsps
,(<++>)
,(<:>)
)where

import Text.Parsec hiding(token)
import Data.Char(isSpace)
import Control.Applicative hiding ((<|>),many)

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b
infixr 5 <:>
infixr 5 <++>

identifier=try( (letter<|>char '_') <:> many(alphaNum<|>char '_') )<?>"identifier"
identifier'=  ( (letter<|>char '_') <:> many(alphaNum<|>char '_') )<?>"identifier"
 
nbsp =satisfy (\x->isSpace x && x/='\n')<?>"non-breaking space"
nbsps=skipMany nbsp

