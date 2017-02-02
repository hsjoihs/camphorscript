{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts , NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  -fno-warn-unused-imports #-}
{- memory allocation -}
module Camphor.Base.Base_Step5
(step5
) where

import Camphor.SafePrelude 
import Camphor.Global.Synonyms
import Camphor.Base.NL_parser
import Text.Parsec hiding(token)
import Camphor.Transformer


step5 :: Stream s Identity Char => FilePath -> s -> Either ParseError Txt
step5 file str = convert5 <$> parse parserNL' (file ++ "--step5") str

convert5 :: [Chunk] -> String
convert5 x = convert5' x ()

convert5' :: [Chunk] -> () -> String
convert5' = map2 c5

map2 :: (a -> StateT n Identity [b]) -> [a] -> n -> [b]
map2 f xs = evalState $ concat <$> mapM f xs

c5 :: Chunk -> StateT () Identity String
c5 (A(INC ,num)) = return $ "inc " ++ showNum num ++ ";"
c5 (A(DEC ,num)) = return $ "dec " ++ showNum num ++ ";"
c5 (A(ASR ,num)) = return $ "assert_zero" ++ showNum num ++ ";"
c5 (B LOOP     ) = return $ "loop;"
c5 (B POOL     ) = return $ "pool;"
c5 (B IN       ) = return   "_input;"
c5 (B OUT      ) = return   "output;"
c5 (C(NUL ,sp) ) = return   sp
c5 (A(MOV ,num)) = return $ "mov " ++ showNum num ++ ";"