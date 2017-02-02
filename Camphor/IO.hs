{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.IO
(outputParsed
,getContentsFrom
,abort
,getDirectoryContents
,doesFileExist
,getArgs
,splitFileName
)where
import Camphor.SafePrelude
import Prelude(error)
import Camphor.Show
import Camphor.Global.Synonyms
import Text.Parsec
import System.FilePath
import System.Directory(getDirectoryContents,doesFileExist)
import System.Environment(getArgs)
import System.IO(hPutStrLn,stderr)

outputParsed :: FilePath -> Either ParseError Txt -> IO()
outputParsed path (Right x) 
 | map toLower path == "con" = putStrLn x
 | otherwise                 = writeFile path x
outputParsed _    (Left  e)  = hPutStrLn stderr $ "parse error at " ++ show e

getContentsFrom :: FilePath -> IO Txt
getContentsFrom file 
 | map toLower file == "con" = getContents
 | otherwise                 = do
  b <- doesFileExist file
  if b 
   then readFile file 
   else abort(file++": No such file")
 
abort :: String -> a
abort = error