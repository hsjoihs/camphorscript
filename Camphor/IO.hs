{-# OPTIONS -Wall #-}
module Camphor.IO
(outputParsed
,Options
,getContentsFrom
,readFile'
,abort
,replaceExtension
,getDirectoryContents
,getArgs
)where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,undefined)
import Camphor.Global
import Text.Parsec
import System.FilePath
import System.Directory(getDirectoryContents)
import System.Environment(getArgs)
import Data.Char(toLower)
type Options = [String]


outputParsed :: FilePath -> Either ParseError Txt -> IO()
outputParsed path (Right x) 
 | map toLower path == "con" = putStrLn x
 | otherwise                 = writeFile path x
outputParsed _    (Left  e)  = putStrLn $ "parse error at " ++ show e

getContentsFrom :: FilePath -> IO Txt
getContentsFrom file = do
 contents <- readFile' file
 return contents
 
readFile' :: FilePath -> IO String
readFile' file 
 | map toLower file == "con" = getContents
 | otherwise                 = readFile file  
 
abort :: String -> a
abort = error