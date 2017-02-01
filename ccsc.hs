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
import Camphor.IO

type Options=[String]

foldl1_::(a->a->a)->[a]->a
foldl1_ _ [] = error "first number of option -C must not be larger than the second"
foldl1_ f xs = foldl1 f xs


info::[String]
info=[
 "CHAtsFtD CamphorScript Compiler - Copyright (c) 2014- CHAtsFtD ",
 "Usage: ccsc [options] [-o outfilepath] infile",
 "options: ",
 "-Cnum[num]  compile from step 'num' to step 'num'"
 ]
 
main::IO()
main=do{args<-getArgs;dispatch2(reverse args)} -- reverse args for pattern matching

step::[String->Either ParseError String]   
step=[step1,undefined,undefined,step4,step5,step6,step7,step8]

fromTo::Int->Int->[a]->[a]
fromTo x y xs = drop(x-1)$take y xs
-- starts with xth(1-indexed) and ends with yth(1-indexed)

fromTo'::Monad m=>Int->Int->[a->m a]->a->m a
fromTo' x y xs=foldl1_ (>=>)(fromTo x y xs)


dispatch2::[String]->IO ()
dispatch2 []                = mapM_ putStrLn info
dispatch2 (infile:ars) = do
 handle  <- openFile infile ReadMode
 contents<- hGetContents handle
 let outfile = outFileOf ars infile
 dispatch3 (reverse$optionsOf ars) outfile contents --options are parsed from the left; thus reverse
 
dispatch3::Options->FilePath->String->IO()
dispatch3 [             ] out cont = outputParsed out (fromTo' 4              8               step cont);
dispatch3 [['-','C',x,y]] out cont = outputParsed out (fromTo' (read[x]::Int) (read[y]::Int)  step cont);
dispatch3 [x            ] _   _    = error$"unknown option"++show x;
dispatch3 xs              _   _    = error$"unknown options"++show xs;

outputParsed::FilePath->Either ParseError String->IO()
outputParsed path (Right x)=writeFile path x
outputParsed _    (Left  e)=putStrLn$"parse error at "++show e

outFileOf::Options->FilePath->FilePath  
outFileOf(y:"-o":_)_      = y
outFileOf _        infile = remExt infile++"bf"

optionsOf::Options->Options
optionsOf(_:"-o":xs) = xs
optionsOf xs         = xs     