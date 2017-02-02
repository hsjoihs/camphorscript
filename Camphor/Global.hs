{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.Global
(
alphaBar,alphaNumBar,identifier,identifier'
,nbsp,nbsps,nbnls,nbnl
,spaces',spaces1',space',newline'
,uint,byte,uint'
,blockComm,lineComm,operator,strP



)where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)


import Camphor.Global.Parsers