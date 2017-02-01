{-# OPTIONS -Wall #-}

import System.Environment
import System.IO
import Control.Monad
import Text.Parsec

import Camphor.Step1
import Camphor.Step4
import Camphor.Step5
import Camphor.Step6
import Camphor.Step7
import Camphor.Step8


foldl1_::(a->a->a)->[a]->a
foldl1_ _ [] = error "first number of option -C must not be larger than the second"
foldl1_ f xs = foldl1 f xs


at::[a]->Int->Maybe a
xs `at` n
 | n>=0      = if length xs>n    then Just(xs!!n)          else Nothing
 | otherwise = if length xs+n>=0 then xs `at` (length xs+n)else Nothing

 
info::[String]
info=[
 "CHAtsFtD CamphorScript Compiler - Copyright (c) 2014- CHAtsFtD ",
 "Usage: ccsc [options] [-o outfilepath] infile",
 "options: ",
 "-Cnum[num]  compile from step 'num' to step 'num'"
 ]

 
remExt::String->String
remExt xs=reverse$dropWhile(/='.')(reverse xs)
 
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


step::[String->Either ParseError String]   
step=[step1,undefined,undefined,step4,step5,step6,step7,step8]

dispatch::[String]->String->Either ParseError String
dispatch []                = Right  -- FIXME: no option, no compile
dispatch ['-': 'C':x:[y]]  = foldl1_(>=>)$drop(read[x]-1::Int)$take (read[y]::Int) step
dispatch [xs]              = error $"unknown option " ++show xs
dispatch xs                = error $"unknown options "++show xs

 