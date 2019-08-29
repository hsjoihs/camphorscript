{-# LANGUAGE FlexibleContexts, NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base.Base_Step2.Base.Ident2
(Ident2(),toIdent2,unId
,bbbb,aaaa,nnnn,readI,writeI,tmpIdent,showIdent
)where
import Camphor.SafePrelude 

newtype Ident2 = Ident2{unId :: String} deriving(Show,Ord,Eq)
 
bbbb, aaaa, nnnn, readI, writeI :: Ident2
bbbb = Ident2 "bbbb"
aaaa = Ident2 "aaaa"
nnnn = Ident2 "NNNN"
readI = Ident2 "read"
writeI = Ident2 "write"

tmpIdent :: Ident2 -> Integer -> Ident2
tmpIdent ident n = Ident2(unId ident ++ "__TMP_" ++ showNum n)


reservedList :: [String]
reservedList = ["char","delete","infixl","infixr","void","constant","const","syntax", "block" ]

toIdent2 :: String -> Either String Ident2
toIdent2 "" = Left ""
toIdent2 i@(x:xs) = maybeToEither i $ do
 guard (isAlpha x || x == '_')
 guard $ null[ a | a <- xs, not (isAlphaNum a), a /= '_']
 guard $ i `notElem` reservedList
 return(Ident2 i)

showIdent :: Ident2 -> String
showIdent = showStr . unId