{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}
{- comment deletion -}
module Camphor.Base_Step8
(step8

,parser8
,convert8
) where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,(!!),read)
import Camphor.Global
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)
import Data.Functor.Identity


step8 :: Stream s Identity Char => FilePath -> s -> Either ParseError String
step8 file str = convert8 <$> (parse parser8 (file++"--step8") str) {-removes unnecessary letters-}

parser8 :: Stream s m Char => ParsecT s u m [String]
parser8 = many char'
 where char' = strP (oneOf "+-<>[],.") <|> (noneOf "+-<>[],." >> return "")
 

convert8 :: [String] -> String
convert8 = concat