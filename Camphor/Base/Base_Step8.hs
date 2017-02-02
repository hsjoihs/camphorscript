{-# LANGUAGE FlexibleContexts, NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
{- comment deletion -}
module Camphor.Base.Base_Step8
(step8

-- ,parser8
-- ,convert8
) where
import Camphor.SafePrelude
import Camphor.Global.Parsers
import Text.Parsec hiding(token)


step8 :: Stream s Identity Char => FilePath -> s -> Either ParseError String
step8 file str = concat <$> parse parser8 (file++"--step8") str {-removes unnecessary letters-}

parser8 :: Stream s m Char => ParsecT s u m [String]
parser8 = many char'
 where char' = strP (oneOf "+-<>[],.") <|> (noneOf "+-<>[],." >> return "")