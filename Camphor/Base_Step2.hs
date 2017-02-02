{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
{- Functional macro expansion -}
module Camphor.Base_Step2
(step2
--,parser2'

) where


import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Partial
import Camphor.Base_Step2_2
import Camphor.Global.Synonyms
import Text.Parsec 
import Data.Functor.Identity

step2 ::  FilePath -> Txt -> Either ParseError Txt
step2 file txt = do
 xs <- parse parser2'  (file ++ "-step2"  ) txt
 ys <- parse parser2_2 (file ++ "-step2-2") xs
--zs <- parse parser2_3 (file ++ "-step2-3") ys
 return(show ys) 

type UserState = () 
 
parser2_3 :: Stream s Identity Sent => ParsecT s u Identity Txt
parser2_3 = do
 xs <- many sent
 return(concat xs)

sent :: Stream s Identity Sent => ParsecT s u Identity Txt
sent = undefined