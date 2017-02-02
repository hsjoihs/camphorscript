{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.Global.Utilities
(makeErr
,Message(..)
,escStar
,newErrorMessage
,conflict
)where
import Camphor.SafePrelude 
import Text.Parsec hiding(token)
import Text.Parsec.Error(newErrorMessage,Message(..))
import Text.Parsec.Pos(newPos)
import qualified Data.Set as Set
-- import Camphor.NonEmpty

makeErr :: Message -> SourceName -> Line -> Column -> Either ParseError b
makeErr msg pos x y = Left$newErrorMessage msg (newPos pos x y) 





escStar :: Char -> String
escStar '*' = "_star_"
escStar x   = [x]

conflict :: (Ord a) => [a] -> Bool
conflict xs = (length . Set.toList . Set.fromList) xs /= length xs