{-# OPTIONS -Wall #-}

import System.Environment
import Control.Monad
import Text.Parsec
import Camphor.IO
import CamphorR.R_Step8


info :: [String]
info = [
 "CHAtsFtD CamphorScript Reverse Compiler - Copyright (c) 2014- CHAtsFtD ",
 "Usage: ccsrc [options] [-o outfilepath] infile",
 "options: ",
 "-Cnum[num]  reverse compile from step 'num' to step 'num'"
 ]

step :: [String -> Either ParseError String]
step=[step8_R]

fromTo'::Monad m=>Int->Int->[a->m a]->a->m a
fromTo' x y xs
 | x<y       = abort "first number of option -C must not be smaller than the second"
 | x>8       = abort$"step "++show x++"does not exist"
 | y<1       = abort$"step "++show y++"does not exist"
 | otherwise = foldl1 (>=>)(drop (8-x) $ take(9-y) $ xs)

main :: IO()
main = do
 args<-getArgs
 dispatch4 args

dispatch4 :: Options -> IO ()
dispatch4 []     = mapM_ putStrLn info
dispatch4 xs     = dispatch5 xs (Nothing,Nothing,(8,8))

type Stat = (Maybe FilePath,Maybe FilePath,(Int,Int)) -- in,out,from,to

dispatch5 :: Options -> Stat -> IO () 
dispatch5 ("-o":outf:xs)    (inf        ,_   ,frmTo     ) = dispatch5 xs (inf,Just outf,frmTo)
dispatch5 ["-o"]             _                            = abort("argument to '-o' is missing")
dispatch5 (['-','C',x,y]:xs)(inf        ,outf,_         ) = dispatch5 xs (inf,outf      ,(read[x],read[y]))
dispatch5 (['-','C',x]  :xs)(inf        ,outf,_         ) = dispatch5 xs (inf,outf      ,(read[x],read[x]))
dispatch5 (inf:xs)          (_          ,outf,frmTo     ) = dispatch5 xs (Just inf,outf      ,frmTo)
dispatch5 []                (Just infile,outf,(a,b)) = do
   contents <- getContentsFrom infile
   outputParsed (maybe (remExt infile++"bf") id outf) (fromTo' a  b step contents)

dispatch5 []                (Nothing    ,_   ,_        )  = abort "no input files"