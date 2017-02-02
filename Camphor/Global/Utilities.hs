{-# OPTIONS -Wall #-}
module Camphor.Global.Utilities
(makeErr,Message(..),readEith,readMay,escStar
)where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Text.Parsec hiding(token)
import Text.Parsec.Error(newErrorMessage,Message(..))
import Text.Parsec.Pos(newPos)

makeErr :: Message -> SourceName -> Line -> Column -> Either ParseError b
makeErr msg pos x y = Left$newErrorMessage msg (newPos pos x y) 

readEith :: Read a => e -> String -> Either e a
readEith err s = case [x | (x,t) <- reads s, ("","") <- lex t] of [x] -> Right x; _ -> Left err

readMay :: Read a => String -> Maybe a
readMay s      = case [x | (x,t) <- reads s, ("","") <- lex t] of [x] -> Just x ; _ -> Nothing 

escStar :: Char -> String
escStar '*' = "_star_"
escStar x   = [x]