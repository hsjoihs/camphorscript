{-# OPTIONS -Wall #-}
module Camphor.IO
(at
,remExt
)where

at::[a]->Int->Maybe a
xs `at` n
 | n>=0      = if length xs>n    then Just(xs!!n)          else Nothing
 | otherwise = if length xs+n>=0 then xs `at` (length xs+n)else Nothing
 
 
remExt::String->String
remExt xs=reverse$dropWhile(/='.')(reverse xs)