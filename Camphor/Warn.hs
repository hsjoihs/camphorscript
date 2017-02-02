{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.Warn
(WarnLevel(..)
,Warning()
,Warnings
,warn
,pretty
,toList',fromList
,tellOne
)where
import Camphor.SafePrelude
import Text.Parsec(SourcePos)
import Camphor.Listlike
import Camphor.Show
import Camphor.Transformer

data Warning = Warn String WarnLevel SourcePos
newtype DList a = DL{unDL :: [a] -> [a]}
type Warnings = DList Warning

tellOne :: (Monad m) => a -> WriterT (DList a) m ()
tellOne a = tell(fromList[a])

fromList :: [a] -> DList a
fromList xs = DL(++xs)

warn :: String -> WarnLevel -> SourcePos -> Warning
warn = Warn

pretty :: WarnLevel -> Warning -> String
pretty limit (Warn str level pos) 
 | limit > level = ""
 | otherwise     = "warning (" ++ p level ++ ") at " ++ show pos ++ ":\n" ++ str ++ "\n"
 where 
  p Crucial = "crucial"
  p Important = "important"
  p Helpful = "helpful"
  p Verbose = "verbose"

instance Monoid(DList a) where
 mempty = DL id
 mappend a b = DL (unDL b . unDL a) -- I don't know why, but this somehow works
 
instance Listlike DList where
 toList' = ($[]) . unDL

data WarnLevel = Verbose | Helpful | Important | Crucial deriving(Show,Eq,Ord)