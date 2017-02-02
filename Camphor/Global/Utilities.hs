{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.Global.Utilities
(makeErr,Message(..),readEith,readMay,escStar,newErrorMessage,remSpace,conflict,lastEq
)where
import Camphor.SafePrelude 
import Text.Parsec hiding(token)
import Text.Parsec.Error(newErrorMessage,Message(..))
import Text.Parsec.Pos(newPos)
import Data.Char(isSpace)
import qualified Data.Set as Set
import Camphor.NonEmpty

makeErr :: Message -> SourceName -> Line -> Column -> Either ParseError b
makeErr msg pos x y = Left$newErrorMessage msg (newPos pos x y) 

readEith :: Read a => e -> String -> Either e a
readEith err s = case [x | (x,t) <- reads s, ("","") <- lex t] of [x] -> Right x; _ -> Left err

readMay :: Read a => String -> Maybe a
readMay s      = case [x | (x,t) <- reads s, ("","") <- lex t] of [x] -> Just x ; _ -> Nothing 

escStar :: Char -> String
escStar '*' = "_star_"
escStar x   = [x]

remSpace :: String -> String
remSpace = filter(not . isSpace)

conflict :: (Ord a) => [a] -> Bool
conflict xs = (length . Set.toList . Set.fromList) xs /= length xs

lastEq :: (Eq a) => [a] -> a -> Bool
lastEq []     _ = False
lastEq (x:xs) y = last' (x:|xs) == y
