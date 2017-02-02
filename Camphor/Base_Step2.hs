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
convert2 file ts = convert2' file (NULL,()) ts -- Right(show ts) {- for debug -}


data ParseState = 
 NULL | Def1 | Def2 Ident | Del1 | Del2 Ident |
 Ifxl1 | Ifxl2 Integer | Ifxl3 Integer | Ifxl4 Integer String | Ifxl5 Integer String |
 Ifxr1 | Ifxr2 Integer | Ifxr3 Integer | Ifxr4 Integer String | Ifxr5 Integer String 
 deriving(Show) 

type OtherStates = ()
type Parser2State = (ParseState,OtherStates)

convert2' :: FilePath -> Parser2State -> [Tok] -> Either ParseError Txt


-- empty etc.
convert2' _    (NULL     ,_) []              = Right ""
convert2' path (stat     ,s)(SP sp      :xs) = (sp++)                    <$>convert2' path (stat     ,s)xs
convert2' path (stat     ,s)(COMM sp    :xs) = (("/*"++sp++"*/")++)      <$>convert2' path (stat     ,s)xs --


-- char a;
convert2' path (NULL     ,s)(CHAR       :xs) =                              convert2' path (Def1     ,s)xs
convert2' path (Def1     ,s)(IDENT idnt :xs) =                              convert2' path (Def2 idnt,s)xs
convert2' path (Def2 idnt,s)(SCOLON     :xs) = (("char "++idnt++";")++)  <$>convert2' path (NULL     ,s)xs --

convert2' path (Def1     ,_)(x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 
convert2' path (Def2 _   ,_)(x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 


-- delete a;
convert2' path (NULL     ,s)(DELETE     :xs) =                              convert2' path (Del1     ,s)xs
convert2' path (Del1     ,s)(IDENT idnt :xs) =                              convert2' path (Del2 idnt,s)xs
convert2' path (Del2 idnt,s)(SCOLON     :xs) = (("delete "++idnt++";")++)<$>convert2' path (NULL     ,s)xs --

convert2' path (Del1     ,_)(x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 
convert2' path (Del2 _   ,_)(x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 


-- ;
convert2' path (NULL     ,s)(SCOLON     :xs) = (";"++)                   <$>convert2' path (NULL     ,s)xs


-- infixl
convert2' path (NULL      ,s)(INFIXL     :xs) =                              convert2' path (Ifxl1     ,s)xs
convert2' path (Ifxl1     ,s)(NUM nm     :xs) =                              convert2' path (Ifxl2 nm  ,s)xs
convert2' path (Ifxl2 n   ,s)(PAREN      :xs) =                              convert2' path (Ifxl3 n   ,s)xs
convert2' path (Ifxl3 n   ,s)(OP op      :xs) =                              convert2' path (Ifxl4 n op,s)xs
convert2' path (Ifxl4 n op,s)(NERAP      :xs) =                              convert2' path (Ifxl5 n op,s)xs
convert2' path (Ifxl5 n op,s)(SCOLON     :xs) =                              convert2' path (NULL      ,q)xs 
 where q = undefined s n op

convert2' path (Ifxl1     ,_)(x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 
convert2' path (Ifxl2 _   ,_)(x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 
convert2' path (Ifxl3 _   ,_)(x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 
convert2' path (Ifxl4 _ _ ,_)(x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 
convert2' path (Ifxl5 _ _ ,_)(x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 


-- infixr
convert2' path (NULL      ,s)(INFIXR     :xs) =                              convert2' path (Ifxr1     ,s)xs
convert2' path (Ifxr1     ,s)(NUM nm     :xs) =                              convert2' path (Ifxr2 nm  ,s)xs
convert2' path (Ifxr2 n   ,s)(PAREN      :xs) =                              convert2' path (Ifxr3 n   ,s)xs
convert2' path (Ifxr3 n   ,s)(OP op      :xs) =                              convert2' path (Ifxr4 n op,s)xs
convert2' path (Ifxr4 n op,s)(NERAP      :xs) =                              convert2' path (Ifxr5 n op,s)xs
convert2' path (Ifxr5 n op,s)(SCOLON     :xs) =                              convert2' path (NULL      ,q)xs 
 where q = undefined s n op

convert2' path (Ifxr1     ,_)(x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 
convert2' path (Ifxr2 _   ,_)(x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 
convert2' path (Ifxr3 _   ,_)(x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 
convert2' path (Ifxr4 _ _ ,_)(x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 
convert2' path (Ifxr5 _ _ ,_)(x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 

-- void
convert2' path (NULL     ,s)(VOID       :xs) = undefined path xs s


-- func()
convert2' path (NULL     ,s)(IDENT idnt :xs) = undefined path xs s idnt


-- (func)(a); or (+ =)(a;b);
convert2' path (NULL     ,s)(PAREN      :xs) = undefined path xs s


-- { ... }
convert2' path (NULL     ,s)(BRACE      :xs) = undefined path xs s


-- others(NUM,NERAP,ECARB,CNSTNT,OP,CONST)
convert2' path (NULL     ,_)(x          :_ ) = makeErr (unExpect$show' x)(path++"step2'") 0 1 



{-
data Tok = 
 CHAR  | DELETE | IDENT Ident   |   NUM Integer   |  
 PAREN | NERAP  | BRACE | ECARB | SCOLON | CNSTNT |
 COMM String    |   OP String   | INFIXL | INFIXR |
 VOID  | CONST  |   SP String                       deriving(Show)
-}