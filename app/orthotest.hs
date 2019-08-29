{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

import Camphor.SafePrelude
import Camphor.IO
import CamphorTest.Random
import CamphorTest.TestSingle
import qualified Data.Map as M 

type TestId = String
main :: IO ()
main = do
 args <- getArgs
 case args of
  ["--getver"]     -> interact$takeWhile(`elem`('.':['0'..'9'])).dropWhile(`elem`['a'..'z'])
  ("--single":xs)  -> testSingle xs
  ["--make",fi,n]  -> case readMay n of 
   Just m -> makeRand (words $ "-C48 -o nul examples/__CCS_for" </> fi) m ("auto_"++fi)
   Nothing -> abort "num not num"
  ("--make":_)    -> abort "invalid `--make'"
  _                -> optopatch
 
optopatch :: IO () 
optopatch = do
 sources' <- getDirectoryFiles "examples/__CCS_for"
 tests'   <- getDirectoryFiles "examples/optotest"
 let tests = process tests'; sources = process sources'
 _ <- M.traverseWithKey (aux sources) tests
 return ()
 
aux :: M.Map TestId FilePath -> TestId -> FilePath -> IO ()
aux ss i tf = case M.lookup i ss of
 Nothing -> abort $ "missing test id "++showStr i
 Just f2 -> putStr' ("testing "++i++" : ") >>  testSingle' ("-C48 "++f2++" -o nul")  tf


process :: [(FilePath,FilePath)] -> M.Map TestId FilePath
process ffs = M.fromList $ map (first getTestId) ffs

 
getTestId :: FilePath -> TestId
getTestId ""          = ""
getTestId ('_':'_':_) = ""
getTestId (x:xs)      = x:getTestId xs