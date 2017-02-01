{-# OPTIONS -Wall #-}

import System.Environment
import System.IO
import Control.Monad
import Text.Parsec
import Camphor.IO

info::[String]
info=[
 "CHAtsFtD CamphorScript Reverse Compiler - Copyright (c) 2014- CHAtsFtD ",
 "Usage: ccsrc [options] [-o outfilepath] infile",
 "options: ",
 "-Cnum[num]  reverse compile from step 'num' to step 'num'"
 ]

main::IO()
main=do{args<-getArgs;dispatch2 (reverse args)}

dispatch2::[String]->IO()
dispatch2 []                = mapM_ putStrLn info
dispatch2 args@(infile:ars) = do
 handle  <- openFile infile ReadMode
 contents<- hGetContents handle
 dispatch3(reverse$optionsOf ars)(outFileOf ars infile)contents

dispatch3::Options->FilePath->String->IO()
dispatch3 = undefined 