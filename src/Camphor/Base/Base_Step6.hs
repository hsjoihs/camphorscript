{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts , NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  -fno-warn-unused-imports #-}
{- memory allocation -}
module Camphor.Base.Base_Step6
(step6
) where

import Camphor.SafePrelude 
import Camphor.Global.Synonyms
import Text.Parsec hiding(token)


step6 :: Stream s Identity Char => FilePath -> s -> Either ParseError Txt
step6 file str = convert6 <$> parse parser6 (file ++ "--step6") str

type Chunk = String

parser6 :: Stream s m Char => ParsecT s u m [Chunk]
parser6 = many sentence <* eof

sentence :: Stream s m Char => ParsecT s u m Chunk
sentence = (:[]) <$> anyChar

convert6 :: [Chunk] -> Txt
convert6 = pack . concat