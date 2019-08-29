{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, FlexibleInstances  #-}
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
,outputWarn
,getDirectoryFiles
,putStr'
,IOTxt(..)
)where
import Camphor.SafePrelude
import Prelude(error)
import Camphor.Show
import Text.Parsec
import System.FilePath
import System.Directory(getDirectoryContents,doesFileExist)
import System.Environment(getArgs)
import System.IO(hPutStrLn,stderr,stdout,hFlush)
import Camphor.Warn
import Camphor.IOTxt

 
putStr' :: IOTxt a => a -> IO()
putStr' str = putStr2 str >> hFlush stdout

getDirectoryFiles :: FilePath -> IO [(FilePath,FilePath)]
getDirectoryFiles dir = do
 conts'    <- getDirectoryContents dir  
 filterM (doesFileExist . snd) [ (file,dir </> file) | file <- conts', not("." `isPrefixOf` file)]

outputWarn :: Maybe Int -> WarnLevel -> Warnings -> IO ()
outputWarn wnum lv ws = do
 let res = map (pretty lv) $ toList ws
 let res2 = case wnum of Nothing -> res; Just n -> take n res
 mapM_ (hPutStrLn stderr) res2

outputErr :: ParseError -> IO ()
outputErr e = hPutStrLn stderr $ "parse error at " ++ show e

outputParsed :: IOTxt a => FilePath -> Either ParseError a -> IO(Maybe a)
outputParsed path (Right x) 
 | map toLower path == "con" = putStrLn2 x >> return(Just x)
 | map toLower path == "nul" = return(Just x)
 | otherwise                 = writeFile2 path x >> return(Just x)
outputParsed _    (Left  e)  = outputErr e >> return Nothing 

getContentsFrom :: IOTxt a => FilePath -> IO a
getContentsFrom file 
 | map toLower file == "con" = getContents2
 | otherwise                 = do
  b <- doesFileExist file
  if b 
   then readFile2 file 
   else abort(file++": No such file")
 
abort :: String -> a
abort = error