{-# LANGUAGE FlexibleContexts ,NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{- Desugaring -}
module Camphor.Base.Base_Step4.IdentFactory
(Ident2
,parseId
,unId
) where


import Camphor.SafePrelude 
import Camphor.Global.Parsers
import Text.Parsec hiding(token)

newtype Ident2 = Ident2{unId :: String} deriving(Show,Ord,Eq)

parseId :: Stream s m Char =>  ParsecT s u m Ident2
parseId = do
 x <- identifier -- fixme: doesn't check reversed
 return(Ident2 x)