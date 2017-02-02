{-# OPTIONS -Wall #-}
import qualified Data.Map as M
import Data.Maybe(mapMaybe)

data Query a = Refer a | Destroy a deriving(Show,Eq,Ord)

data Stat a = Stat{ all_ :: M.Map a Integer, now :: M.Map a Integer}

emptyState :: Stat a
emptyState = Stat M.empty M.empty

halfScramble :: (Ord a) => [Query a] -> M.Map a Integer
halfScramble qs = convert $ halfScramble2 emptyState qs

convert :: Stat a -> M.Map a Integer
convert = convert

halfScramble2 :: (Ord a) => Stat a -> [Query a] -> Stat a
halfScramble2 s                []             = s
halfScramble2 s@Stat{now = st} (Destroy a:xs) = halfScramble2 s{now = M.delete a st} xs -- delete
halfScramble2 s                (Refer a:xs)   = undefined
 where 
  (before,after) = break (==Destroy a)xs
  
  
refMay :: Query a -> Maybe a
refMay (Refer a) = Just a
refMay _         = Nothing
  
isRef :: Query a -> Bool
isRef (Refer _) = True
isRef _         = False
  
-- is :: (Eq a) => Query a -> a -> Bool
-- Refer a `is` b = a==b
-- Destroy a `is` b = a==b