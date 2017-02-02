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
,outputErr
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

outputErr :: ParseError -> IO ()
outputErr e = hPutStrLn stderr $ "parse error at " ++ show e

outputParsed :: FilePath -> Either ParseError Txt -> IO(Maybe Txt)
outputParsed path (Right x) 
 | map toLower path == "con" = putStrLn x >> return(Just x)
 | map toLower path == "nul" = return(Just x)
 | otherwise                 = writeFile path x >> return(Just x)
outputParsed _    (Left  e)  = outputErr e >> return(Nothing)

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