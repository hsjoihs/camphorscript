{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall  #-}
{- symbolization -}
module Camphor.Base.Base_Step7
(step7
-- ,parserND'
-- ,convert7
-- ,convert7'
) where
import Camphor.SafePrelude 
import Camphor.Global.Synonyms
import Text.Parsec hiding(token)
import Camphor.Base.ND_parser
import Camphor.Transformer

step7 :: Stream s Identity Char => FilePath -> s -> Either ParseError Txt
step7 file str = convert7 <$> parse parserND' (file ++ "--step7") str

convert7 :: [Chunk] -> Txt
convert7 x = convert7' x 0

convert7' :: [Chunk] -> Integer -> Txt
convert7' = map2 (fmap pack . c7)

map2 :: (Monoid b) => (a -> StateT n Identity b) -> [a] -> n -> b
map2 f xs = evalState $ mconcat <$> mapM f xs

c7 :: Chunk -> StateT Integer Identity String
c7 (A(INC ,num)) = return $ genericTake num(repeat '+')
c7 (A(DEC ,num)) = return $ genericTake num(repeat '-')
c7 (A(ASR ,_  )) = return   ""
c7 (B LOOP     ) = return   "["
c7 (B POOL     ) = return   "]"
c7 (B IN       ) = return   ","
c7 (B OUT      ) = return   "."
c7 (C(NUL ,sp) ) = return   sp
c7 (A(MOV ,num)) = do
 n <- get
 put num
 return $ if n <= num then genericTake(num-n)(repeat '>') else genericTake(n-num)(repeat '<')