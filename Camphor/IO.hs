{-# OPTIONS -Wall #-}
module Camphor.IO
(outputParsed
,Options
,getContentsFrom
,abort
,replaceExtension
)where
import Camphor.Global
import Text.Parsec
import System.IO
import System.FilePath

type Options=[String]


outputParsed :: FilePath->Either ParseError Txt->IO()
outputParsed path (Right x)=writeFile path x
outputParsed _    (Left  e)=putStrLn$"parse error at "++show e



getContentsFrom :: FilePath -> IO Txt
getContentsFrom file = do
 handle   <- openFile file ReadMode
 contents <- hGetContents handle
 return contents
 
abort :: String -> a
abort = error