{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{- symbolization -}
module Camphor.Base_Step7
(step7

,Com7(..)
,parserND'
,convert7
,convert7'
) where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Global
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)
import Data.List(genericTake)
import Data.Functor.Identity
import Camphor.ND_parser


step7 :: Stream s Identity Char => FilePath -> s -> Either ParseError Txt
step7 file str=convert7 <$> (parse parserND' (file++"--step7") str)  


convert7 :: [Chunk]->String
convert7 x = convert7'(0,x)

  
convert7' :: (Integer,[Chunk])->String
convert7' (_,[]             )  = ""
convert7' (n,(Left (INC ,num):xs))  = genericTake num(repeat '+') ++ convert7'(n,xs)
convert7' (n,(Left (DEC ,num):xs))  = genericTake num(repeat '-') ++ convert7'(n,xs)
convert7' (n,(Right(LOOP,_  ):xs))  = "["                                        ++ convert7'(n,xs)
convert7' (n,(Right(POOL,_  ):xs))  = "]"                                        ++ convert7'(n,xs)
convert7' (n,(Right(IN  ,_  ):xs))  = ","                                        ++ convert7'(n,xs)
convert7' (n,(Right(OUT ,_  ):xs))  = "."                                        ++ convert7'(n,xs)
convert7' (n,(Right(NUL ,sp ):xs))  = sp                                         ++ convert7'(n,xs)
convert7' (n,(Left (MOV ,num):xs))  
 |                       n<=num     = genericTake(num-n)(repeat '>')             ++ convert7'(num,xs)
 |                       otherwise  = genericTake(n-num)(repeat '<')             ++ convert7'(num,xs)