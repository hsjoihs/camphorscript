{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

import Camphor.SafePrelude
import Text.Parsec
import Camphor.Global.Utilities
import Camphor.Global.Operators
import Camphor.Global.Synonyms
import Camphor.Step1
import Camphor.Step2
import Camphor.Step3_I
import Camphor.Step3_II
import Camphor.Step5
import Camphor.Step6
import Camphor.Step7
import Camphor.Step8
import Camphor.IO
import Camphor.Lib
import Data.List(isPrefixOf)
import qualified Data.Map as M

ioLibs :: IO [FilePath]
ioLibs = do
 libs' <- getDirectoryContents lib_dir
 return [ file | file <- libs', not("." `isPrefixOf` file)]

getLibs3 :: IO(M.Map FilePath Txt) 
getLibs3 = do
  libs <- ioLibs
  texts <- mapM getContentsFrom (map (lib_dir </>) libs)
  return ( M.fromList $ zip libs texts)

info::[String]
info=[
 "CHAtsFtD CamphorScript Compiler - Copyright (c) 2014- CHAtsFtD ",
 "Usage: ccsc [options] [-o outfilepath] infile",
 "options: ",
 "-Cnum[num]  compile from step 'num' to step 'num'",
 "-E          C preprocess only",
 "-m num      limit the number of memory used in the compiled Brainf*ck",
 "con         uses stdin or stdout instead of infile or outfile"
 ]
 
main :: IO()
main = do
 args <- getArgs
 dispatch4 args

step :: FilePath -> (M.Map FilePath Txt) -> Maybe MemSize -> [Txt -> Either ParseError Txt]   
step file includer mem= map ($file) [step1 includer,step2,step3_I,step3_II mem,step5,step6,step7,step8]

-- starts with xth(1-indexed) and ends with yth(1-indexed)
fromTo' :: Monad m => Int -> Int -> [a -> m a] -> a -> m a
fromTo' x y xs
 | x<1       = abort("step "++show x++"does not exist")
 | y>8       = abort("step "++show y++"does not exist")
 | otherwise = case (drop(x-1)$take y xs) of 
  (t:ts) -> foldl (>=>) t ts
  _ -> abort "first number of option -C must not be larger than the second"

dispatch4 :: Options -> IO ()
dispatch4 [] = mapM_ putStrLn info
dispatch4 xs = dispatch5 xs (Nothing,Nothing,Right(4,8),Nothing)

type Stat = (Maybe FilePath,Maybe FilePath,Either String(Int,Int),Maybe MemSize) -- in,out,from,to,memoryNum

dispatch5 :: Options -> Stat -> IO () 
dispatch5 ("-o":outf:xs)      (inf        ,_   ,frmTo     ,mem) = dispatch5 xs (     inf,Just outf,frmTo,mem)
dispatch5 ["-o"]               _                                = abort("argument to '-o' is missing")
dispatch5 ("-m":mem :xs)      (inf        ,outf,frmTo     ,_  ) = case readMay mem of
 Just mem' -> dispatch5 xs (     inf,     outf,frmTo,Just mem')
 Nothing   -> abort("argument to '-m' is not an integer")
dispatch5 ["-m"]               _                                = abort("argument to '-m' is missing")
dispatch5 (o@['-','C',x,y]:xs)(inf        ,outf,_         ,mem) = case (readMay[x],readMay[y]) of
 (Just x',Just y') -> dispatch5 xs (inf,outf,Right(x',y'),mem)
 _                 -> abort ("incorrect format "++show o++" of option -Cnum[num]")    
  
dispatch5 (p@['-','C',x]  :xs)(inf        ,outf,_         ,mem) = case readMay [x] of 
 Just x'           -> dispatch5 xs (inf,outf,Right(x',x'),mem)
 Nothing           -> abort ("incorrect format "++show p++" of option -Cnum[num]")

dispatch5 ("-E":xs)           (inf        ,outf,_         ,mem) = dispatch5 xs (     inf,outf      ,Right(1,1),mem)
dispatch5 (inf:xs)            (_          ,outf,frmTo     ,mem) = dispatch5 xs (Just inf,outf      ,frmTo     ,mem)

dispatch5 []                  (_          ,_   ,Left _    ,_  ) = return ()
dispatch5 []                  (Nothing    ,_   ,_         ,_  ) = abort "no input file"
dispatch5 []                  (Just infile,Just outf,Right(a,b),mem) = do
   contents <- getContentsFrom infile
   includer <- getLibs3
   outputParsed outf (fromTo' a  b (step infile includer mem) contents)
dispatch5 []                  (_          ,Nothing,_,_) = abort "no output file"