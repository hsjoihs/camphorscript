{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.Global.Utilities
(makeErr,Message(..),readEith,readMay,escStar,newErrorMessage,remSpace,conflict,lastEq,maybeToEither,forStatM
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

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _   (Just x) = Right x
maybeToEither err Nothing  = Left err

readEith :: Read a => e -> String -> Either e a
readEith err = maybeToEither err . readMay 

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

forStatM :: (Monad m) => [a] -> (a -> b -> m (c,b)) -> b -> m ([c],b) 
forStatM [] _ stat = return([],stat)
forStatM (x:xs) f stat = do
 (c,newStat) <- f x stat  
 (cs,lastStat) <- forStatM xs f newStat
 return(c:cs,lastStat)
