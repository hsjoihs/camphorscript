{-# OPTIONS -Wall #-}
module Camphor.IO
(remExt
,outputParsed
,outFileOf
,optionsOf
,Options
,getContentsFrom
)where
import Text.Parsec
import System.IO

type Options=[String]

remExt :: String->String
remExt xs=reverse$dropWhile(/='.')(reverse xs)

outputParsed :: FilePath->Either ParseError String->IO()
outputParsed path (Right x)=writeFile path x
outputParsed _    (Left  e)=putStrLn$"parse error at "++show e

outFileOf :: Options->FilePath->FilePath  
outFileOf(y:"-o":_)_      = y
outFileOf _        infile = remExt infile++"bf"

optionsOf :: Options->Options
optionsOf(_:"-o":xs) = xs
optionsOf xs         = xs

getContentsFrom :: FilePath -> IO String
getContentsFrom file = do
 handle   <- openFile file ReadMode
 contents <- hGetContents handle
 return contents