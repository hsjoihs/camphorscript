{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind -fno-warn-unused-imports #-}
module CamphorR.R_Base_Step4
(step4_R


)where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error)
import Camphor.Global
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)
import Text.Parsec.Error
import Text.Parsec.Pos
import Camphor.ND_parser

step4_R :: String -> Either ParseError String
step4_R str= do
 parsed <- parse parserND' ("Rstep4") str
 convert4_R parsed

convert4_R :: [Chunk] -> Either ParseError String
convert4_R = undefined