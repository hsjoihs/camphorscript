{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.CmdOptions
(Stat,optionParse,info
)where
import Camphor.SafePrelude
import Camphor.Global.Utilities
import Camphor.Global.Synonyms

type Stat = (Maybe FilePath,Maybe FilePath,(Int,Int),Maybe MemSize) -- in,out,from,to,memoryNum

info :: [String]
info = [
 "CHAtsFtD CamphorScript Compiler - Copyright (c) 2014- CHAtsFtD ",
 "Usage: ccsc [options] [-o outfilepath] infile",
 "options: ",
 "-Cnum[num]  compile from step 'num' to step 'num'",
 "-E          C preprocess only",
 "-m num      limit the number of memory used in the compiled Brainf*ck",
 "con         uses stdin or stdout instead of infile or outfile"
 ]

optionParse :: Options -> Stat -> Either String (FilePath,FilePath,Int,Int,Maybe MemSize)
optionParse ("-o":outf:xs)      (inf        ,_        ,frmTo,mem) = optionParse xs (inf,Just outf,frmTo,mem)
optionParse ["-o"]               _                                = Left "argument to '-o' is missing"
optionParse ("-m":mem :xs)      (inf        ,outf     ,frmTo,_  ) = case readMay mem of
 Just mem' -> optionParse xs (inf,outf,frmTo,Just mem')
 Nothing   -> Left "argument to '-m' is not an integer"
optionParse ["-m"]               _                                = Left "argument to '-m' is missing"
optionParse (o@['-','C',x,y]:xs)(inf        ,outf     ,_    ,mem) = case (readMay[x],readMay[y]) of
 (Just x',Just y') -> optionParse xs (inf,outf,(x',y'),mem)
 _                 -> Left ("incorrect format "++show o++" of option -Cnum[num]")  
optionParse (p@['-','C',x]  :xs)(inf        ,outf     ,_    ,mem) = case readMay [x] of 
 Just x'           -> optionParse xs (inf,outf,(x',x'),mem)
 Nothing           -> Left ("incorrect format "++show p++" of option -Cnum[num]")

optionParse ("-E":xs)           (inf        ,outf     ,_    ,mem) = optionParse xs (     inf,outf      ,(1,1),mem)
optionParse (inf :xs)           (_          ,outf     ,frmTo,mem) = optionParse xs (Just inf,outf      ,frmTo,mem)

optionParse []                  (Nothing    ,_        ,_    ,_  ) = Left "no input file"
optionParse []                  (Just infile,Just outf,(a,b),mem) = Right(infile,outf,a,b,mem)
optionParse []                  (_          ,Nothing  ,_    ,_  ) = Left "no output file"