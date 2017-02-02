{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

import Camphor.SafePrelude
import Camphor.IO
import Test.TestSingle

main :: IO ()
main = do
 args <- getArgs
 case args of
  ["--getver"]    -> interact$takeWhile(`elem`('.':['0'..'9'])).dropWhile(`elem`['a'..'z'])
  ("--single":xs) -> testSingle xs
  _               -> optopatch
 
optopatch :: IO () 
optopatch = mapM_ putStrLn info 