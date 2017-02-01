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
main=do
 args<-getArgs
 if(null args)
  then mapM_ putStrLn info
  else do
   let infile =last args
   let outfile=if(args `at`(-3)==Just "-o")then maybe "anonymous.bf" id (args `at`(-2)) else remExt infile++"bf"
   let options=if(args `at`(-3)/=Just "-o")then init args else init$init$init$args
   handle  <- openFile infile ReadMode
   contents<- hGetContents handle
   case dispatch options contents of{Right x->writeFile outfile x;Left x->error(show x)}
   return()
   
dispatch::[String]->String->Either ParseError String
dispatch []                = Right  