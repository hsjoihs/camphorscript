{-# LANGUAGE FlexibleContexts, NoImplicitPrelude #-}
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
import Camphor.ND_parser


step7 :: Stream s Identity Char => FilePath -> s -> Either ParseError Txt
step7 file str = convert7 <$> (parse parserND' (file ++ "--step7") str)  


convert7 :: [Chunk]->String
convert7 x = convert7'(0,x)


convert7' :: (Integer,[Chunk])->String
convert7' (_,[]             )  = ""
convert7' (n,(A(INC ,num):xs))  = genericTake num(repeat '+') ++ convert7'(n,xs)
convert7' (n,(A(DEC ,num):xs))  = genericTake num(repeat '-') ++ convert7'(n,xs)
convert7' (n,(A(ASR ,_  ):xs))  = convert7'(n,xs)
convert7' (n,(B LOOP     :xs))  = "["                                        ++ convert7'(n,xs)
convert7' (n,(B POOL     :xs))  = "]"                                        ++ convert7'(n,xs)
convert7' (n,(B IN       :xs))  = ","                                        ++ convert7'(n,xs)
convert7' (n,(B OUT      :xs))  = "."                                        ++ convert7'(n,xs)
convert7' (n,(C(NUL ,sp ):xs))  = sp                                         ++ convert7'(n,xs)
convert7' (n,(A(MOV ,num):xs))  
 |                   n<=num     = genericTake(num-n)(repeat '>')             ++ convert7'(num,xs)
 |                   otherwise  = genericTake(n-num)(repeat '<')             ++ convert7'(num,xs)