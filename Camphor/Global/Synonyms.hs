{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS -Wall #-}
module Camphor.Global.Synonyms
(Ident
,Txt
,pack,unpack
,MemSize
,Address
,Fix
,Oper()
,wrap
,unOp
,PragmaData
,Options
,Includers
,FileToTxt
,(<+++$$>)
,AddTxt(..)
)where
import Camphor.SafePrelude
import Camphor.Oper
-- import Camphor.IOTxt
-- import Text.Parsec.Prim
import qualified Data.Map as M
import qualified Data.Text.Lazy as T

type Ident = String
-- newtype Txt = T String deriving(Eq,Ord,Monoid,IOTxt)
type Txt = T.Text
pack :: String -> Txt
pack = {-T-} T.pack 

unpack :: Txt -> String
unpack {-T t = t-} = T.unpack


-- instance (Monad m) => Stream Txt m Char where
 -- uncons = return . T.uncons
 -- uncons (T [])     = return $ Nothing
 -- uncons (T (t:ts)) = return $ Just (t,T ts)
(<+++$$>) :: (Functor f, Functor f1) => String -> f (f1 Txt) -> f (f1 Txt) 
a <+++$$> b = pack a <++$$> b 

infixr 5 <+>
class AddTxt a b where
 (<+>) :: a -> b -> Txt

instance AddTxt String String where
 a <+> b = pack a `mappend` pack b
 
instance AddTxt String Txt where
 a <+> b = pack a `mappend` b
 
instance AddTxt Txt String where
 a <+> b = a `mappend` pack b
 
instance AddTxt Txt Txt where
 a <+> b = a `mappend` b

type MemSize = Integer
type Address = Integer
type Fix = Integer
type PragmaData = [String]
type Options = [String]
type FileToTxt = M.Map FilePath (FilePath,Txt)
type Includers = (FileToTxt,FileToTxt,M.Map Ident String)