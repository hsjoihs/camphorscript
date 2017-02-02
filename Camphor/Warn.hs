{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.Warn
(WarnLevel(..)
,Warning()
,Warnings
)where
import Camphor.SafePrelude
import Text.Parsec(SourcePos)

data Warning = Warn String WarnLevel SourcePos
newtype DList a = DL{unDL :: [a] -> [a]}
type Warnings = DList Warning

instance Monoid(DList a) where
 mempty = DL id
 mappend a b = DL (unDL a . unDL b)

data WarnLevel = Crucial | Important | Helpful | Verbose deriving(Show,Eq,Ord)