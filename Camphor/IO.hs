{-# OPTIONS -Wall #-}
module Camphor.IO
(at
,remExt
,outputParsed
,outFileOf
,optionsOf
,Options
)where
import Text.Parsec

type Options=[String]

at::[a]->Int->Maybe a
xs `at` n
 | n>=0      = if length xs>n    then Just(xs!!n)          else Nothing
 | otherwise = if length xs+n>=0 then xs `at` (length xs+n)else Nothing
 
 
remExt::String->String
remExt xs=reverse$dropWhile(/='.')(reverse xs)

outputParsed::FilePath->Either ParseError String->IO()
outputParsed path (Right x)=writeFile path x
outputParsed _    (Left  e)=putStrLn$"parse error at "++show e

outFileOf::Options->FilePath->FilePath  
outFileOf(y:"-o":_)_      = y
outFileOf _        infile = remExt infile++"bf"

optionsOf::Options->Options
optionsOf(_:"-o":xs) = xs
optionsOf xs         = xs