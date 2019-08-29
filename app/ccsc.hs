{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

import Camphor.SafePrelude
import Camphor.IO
import Camphor.Main2
 
main :: IO()
main = do
 args <- getArgs
 dispatch4 args