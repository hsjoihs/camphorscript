{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, FlexibleInstances  #-}
{-# OPTIONS -Wall #-}
module Camphor.IOTxt
(IOTxt(..)
)where
import Camphor.SafePrelude
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

class IOTxt a where
 readFile2 :: FilePath -> IO a
 writeFile2 :: FilePath -> a -> IO ()
 appendFile2 :: FilePath -> a -> IO ()
 getLine2 :: IO a
 getContents2 :: IO a
 putStr2 :: a -> IO ()
 putStrLn2 :: a -> IO ()
 
instance IOTxt String where
 readFile2 = readFile
 writeFile2 = writeFile
 appendFile2 = appendFile
 getLine2 = getLine
 getContents2 = getContents
 putStr2 = putStr
 putStrLn2 = putStrLn

instance IOTxt T.Text where
 readFile2 = T.readFile
 writeFile2 = T.writeFile
 appendFile2 = T.appendFile
 getLine2 = T.getLine
 getContents2 = T.getContents
 putStr2 = T.putStr
 putStrLn2 = T.putStrLn 