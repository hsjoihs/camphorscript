{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{- Desugaring -}
module Camphor.Base_Step3
(step3
,parser3
,convert3

) where


import Camphor.Global
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)

type Set3 = String

step3 :: FilePath -> Txt -> Either ParseError Txt
step3 file str = do
 sets <- parse parser3 (file++"--step3") str
 convert3 file sets
 
parser3 :: Stream s m Char => ParsecT s u m [Set3]
parser3 = many sentences3

sentences3 :: Stream s m Char => ParsecT s u m Set3
sentences3 = def <|> del <|> add <|> sub <|> while <|> block <|> read_ <|> write <|> nul <|> emp <|> comm 
 where 
  def   = undefined
  del   = undefined
  add   = undefined
  sub   = undefined
  while = undefined
  block = undefined
  read_ = undefined
  write = undefined
  nul   = undefined
  emp   = undefined
  comm  = undefined

convert3 :: FilePath -> [Set3] -> Either ParseError Txt
convert3 file sets = undefined file sets