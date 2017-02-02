{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.Global.Operators
((<++>),(<:>),(<$$>),(</>){-,(<++$>),(<++$$>)-}
)where
import Camphor.SafePrelude
import System.FilePath((</>))
infixr 5 <++>
(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

infixl 4 <$$>
(<$$>) :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)
(<$$>) = fmap . fmap 
{-
infixl 4 <++$>
(<++$>) :: Functor f => [a] -> f [a] -> f [a]
a <++$> b = (a++) <$> b

infixl 4 <++$$>
(<++$$>) :: (Functor f, Functor f1) => [a] -> f (f1 [a]) -> f (f1 [a])
a <++$$> b = (a++) <$$> b
-}
-- System.FilePath.</>