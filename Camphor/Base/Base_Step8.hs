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
import Camphor.Global.Synonyms
import Text.Parsec hiding(token)


step8 :: Stream s Identity Char => FilePath -> s -> Either ParseError Txt
step8 file str = mconcat <$> parse parser8 (file++"--step8") str {-removes unnecessary letters-}

parser8 :: Stream s m Char => ParsecT s u m [Txt]
parser8 = many $ pack <$> char'
 where char' = strP (oneOf "+-<>[],.") <|> (noneOf "+-<>[],." >> return "")