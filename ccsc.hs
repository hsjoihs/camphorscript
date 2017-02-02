{-# OPTIONS -Wall #-}

import Text.Parsec
import Camphor.Global
import Camphor.Step1
import Camphor.Step3
import Camphor.Step5
import Camphor.Step6
import Camphor.Step7
import Camphor.Step8
import Camphor.IO
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
 "-E          C preprocess only"
 ]
 
main :: IO()
main = do
 args <- getArgs
 dispatch4 args


step :: FilePath -> (M.Map FilePath Txt) -> [Txt -> Either ParseError Txt]   
step file includer = map ($file) [step1 includer,undefined,undefined,step3,step5,step6,step7,step8]


-- starts with xth(1-indexed) and ends with yth(1-indexed)
fromTo' :: Monad m => Int -> Int -> [a -> m a] -> a -> m a
fromTo' x y xs
 | x>y       = abort "first number of option -C must not be larger than the second"
 | x<1       = abort("step "++show x++"does not exist")
 | y>8       = abort("step "++show y++"does not exist")
 | otherwise = foldl1 (>=>)(drop(x-1)$take y xs)



dispatch4 :: Options -> IO ()
dispatch4 [] = mapM_ putStrLn info
dispatch4 xs = dispatch5 xs (Nothing,Nothing,Right(4,8))

type Stat = (Maybe FilePath,Maybe FilePath,Either String(Int,Int)) -- in,out,from,to

dispatch5 :: Options -> Stat -> IO () 
dispatch5 ("-o":outf:xs)    (inf        ,_   ,frmTo     ) = dispatch5 xs (     inf,Just outf,frmTo)
dispatch5 ["-o"]             _                            = abort("argument to '-o' is missing")
dispatch5 (['-','C',x,y]:xs)(inf        ,outf,_         ) = dispatch5 xs (     inf,outf      ,Right(read[x],read[y]))
dispatch5 (['-','C',x]  :xs)(inf        ,outf,_         ) = dispatch5 xs (     inf,outf      ,Right(read[x],read[x]))
dispatch5 ("-E":xs)         (inf        ,outf,_         ) = dispatch5 xs (     inf,outf      ,Right(1      ,1      ))
dispatch5 (inf:xs)          (_          ,outf,frmTo     ) = dispatch5 xs (Just inf,outf      ,frmTo)

dispatch5 []                (Just infile,outf,Right(a,b)) = do
   contents <- getContentsFrom infile
   includer <- getLibs3
   outputParsed (maybe (replaceExtension infile "bf") id outf) (fromTo' a  b (step infile includer) contents)
   
   
dispatch5 []                (Nothing    ,_   ,_        )  = abort "no input files"
dispatch5 []                (_          ,_   ,Left _)     = return ()