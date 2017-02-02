{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
{- Functional macro expansion -}
module Camphor.Base_Step2
(step2
,parser2'

) where

import Camphor.Partial
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Global
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)
import Camphor.PCS_Parser

step2 :: FilePath -> Txt -> Either ParseError Txt
step2 file str = do
 ts <- parse parser2' (file ++ "-step2") str
 convert2 file ts
 
 
convert2 :: FilePath -> [Tok] -> Either ParseError Txt
convert2 file ts = convert2' file NULL ts -- Right(show ts) {- for debug -}


data Parser2State = NULL | Def1 | Def2 Ident | Del1 | Del2 Ident deriving(Show) 

convert2' :: FilePath -> Parser2State -> [Tok] -> Either ParseError Txt

-- empty etc.
convert2' _     NULL      []               = Right ""
convert2' path  stat      (SP sp      :xs) = (sp++)                    <$>convert2' path  stat        xs
convert2' path  stat      (COMM sp    :xs) = (("/*"++sp++"*/")++)      <$>convert2' path  stat        xs


-- char a;
convert2' path  NULL      (CHAR       :xs) =                              convert2' path  Def1        xs
convert2' path  Def1      (IDENT idnt :xs) =                              convert2' path (Def2 idnt)  xs
convert2' path (Def2 idnt)(SCOLON     :xs) = (("char "++idnt++";")++)  <$>convert2' path  NULL        xs

convert2' path  Def1      (x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 
convert2' path (Def2 _)   (x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 


-- delete a;
convert2' path  NULL      (DELETE     :xs) =                              convert2' path  Del1        xs
convert2' path  Del1      (IDENT idnt :xs) =                              convert2' path (Del2 idnt)  xs
convert2' path (Del2 idnt)(SCOLON     :xs) = (("delete "++idnt++";")++)<$>convert2' path  NULL        xs

convert2' path  Del1      (x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 
convert2' path (Del2 _)   (x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 


-- ;
convert2' path  NULL      (SCOLON     :xs) = (";"++)                   <$>convert2' path  NULL        xs


-- infixl
convert2' path  NULL      (INFIXL     :xs) = undefined


-- infixr
convert2' path  NULL      (INFIXR     :xs) = undefined


-- void
convert2' path  NULL      (VOID       :xs) = undefined


-- func()
convert2' path  NULL      (IDENT idnt :xs) = undefined


-- (func)(a); or (+ =)(a;b);
convert2' path  NULL      (PAREN      :xs) = undefined


-- { ... }
convert2' path  NULL      (BRACE      :xs) = undefined


-- others(NUM,NERAP,ECARB,CNSTNT,OP,CONST)
convert2' path  NULL      (x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 



