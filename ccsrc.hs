{-# OPTIONS -Wall #-}

import System.Environment
import System.IO
import Control.Monad
import Text.Parsec
import Camphor.IO
import CamphorR.R_Step8

info::[String]
info=[
 "CHAtsFtD CamphorScript Reverse Compiler - Copyright (c) 2014- CHAtsFtD ",
 "Usage: ccsrc [options] [-o outfilepath] infile",
 "options: ",
 "-Cnum[num]  reverse compile from step 'num' to step 'num'"
 ]

step::[String->Either ParseError String]
step=[step8_R]

fromTo::Int->Int->[a]->[a]
fromTo x y xs = drop(8-x)$take(9-y)$xs

fromTo'::Monad m=>Int->Int->[a->m a]->a->m a
fromTo' x y xs
 | x<y       = error "first number of option -C must not be smaller than the second"
 | x>8       = error$"step "++show x++"does not exist"
 | y<1       = error$"step "++show y++"does not exist"
 | otherwise = foldl1 (>=>)(fromTo x y xs)

main::IO()
main=do{args<-getArgs;dispatch2 (reverse args)}

dispatch2::[String]->IO()
dispatch2 []                = mapM_ putStrLn info
dispatch2 (infile:ars) = do
 handle  <- openFile infile ReadMode
 contents<- hGetContents handle
 dispatch3(reverse$optionsOf ars)(outFileOf ars infile)contents

dispatch3::Options->FilePath->String->IO()
dispatch3 [             ] out cont = outputParsed out (fromTo' 8              8               step cont);
dispatch3 [['-','C',x,y]] out cont = outputParsed out (fromTo' (read[x]::Int) (read[y]::Int)  step cont);
dispatch3 [['-','C',x]  ] out cont = outputParsed out (fromTo' (read[x]::Int) (read[x]::Int)  step cont);
dispatch3 [x            ] _   _    = error$"unknown option"++show x;
dispatch3 xs              _   _    = error$"unknown options"++show xs;