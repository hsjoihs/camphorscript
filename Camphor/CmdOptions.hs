{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.CmdOptions
(Stat,optionParse,info
)where
import Camphor.SafePrelude
import Camphor.Global.Utilities
import Camphor.Global.Synonyms

type Stat = (Maybe FilePath,Maybe FilePath,(Int,Int),Maybe MemSize,[FilePath],[FilePath],Bool,Bool) -- infile,-o,from,to,-m,-I,-L,-nostdinc,-nostdlib

info :: [String]
info = [
 "CHAtsFtD CamphorScript Compiler - Copyright (c) 2014- CHAtsFtD ",
 "Usage: ccsc [options] [-o outfilepath] infile",
 "options: ",
 "-Cnum[num]  compile from step 'num' to step 'num'",
 "-E          C preprocess only",
 "-Idir       add include path 'dir'",
 "-Ldir       add library path 'dir'",
 "-nostdinc   do not use standard include paths (current directory)",
 "-nostdlib   do not use standard library paths ",
 "-m num      limit the number of memory used in the compiled Brainf*ck",
 "con         uses stdin or stdout instead of infile or outfile"
 ]

optionParse :: Options -> Stat -> Either String (FilePath,FilePath,(Int,Int),Maybe MemSize,[FilePath],[FilePath],Bool,Bool)
optionParse ("-o":outf              :xs)(inf        ,_        ,frmTo,mem,fd,ld,ni,nl) = optionParse xs (inf,Just outf,frmTo,mem,fd,ld,ni,nl)
optionParse ["-o"]               _                                            = Left "argument to '-o' is missing"
optionParse ("-m":mem               :xs)(inf        ,outf     ,frmTo,_  ,fd,ld,ni,nl) = case readMay mem of
 Just mem' -> optionParse xs (inf,outf,frmTo,Just mem',fd,ld,ni,nl)
 Nothing   -> Left "argument to '-m' is not an integer"
optionParse ["-m"]               _                                            = Left "argument to '-m' is missing"
optionParse (o@['-','C',x,y]        :xs)(inf        ,outf     ,_    ,mem,fd,ld,ni,nl) = case (readMay[x],readMay[y]) of
 (Just x',Just y') -> optionParse xs (inf,outf,(x',y'),mem,fd,ld,ni,nl)
 _                 -> Left ("incorrect format "++show o++" of option -Cnum[num]")  
optionParse (p@['-','C',x]          :xs)(inf        ,outf     ,_    ,mem,fd,ld,ni,nl) = case readMay [x] of 
 Just x'           -> optionParse xs (inf,outf,(x',x'),mem,fd,ld,ni,nl)
 Nothing           -> Left ("incorrect format "++show p++" of option -Cnum[num]")
optionParse (('-':'I':(dir@(_:_)))  :xs)(inf        ,outf     ,frmTo,mem,fd,ld,ni,nl) = optionParse xs (     inf,outf,frmTo,mem,fd++[dir],ld,ni,nl)
optionParse (('-':'L':(dir@(_:_)))  :xs)(inf        ,outf     ,frmTo,mem,fd,ld,ni,nl) = optionParse xs (     inf,outf,frmTo,mem,fd,ld++[dir],ni,nl)
optionParse ("-I":dir:xs)               (inf        ,outf     ,frmTo,mem,fd,ld,ni,nl) = optionParse xs (     inf,outf,frmTo,mem,fd++[dir],ld,ni,nl)
optionParse ("-L":dir:xs)               (inf        ,outf     ,frmTo,mem,fd,ld,ni,nl) = optionParse xs (     inf,outf,frmTo,mem,fd,ld++[dir],ni,nl)
optionParse ["-I"]               _                                                    = Left "argument to '-I' is missing"
optionParse ["-L"]               _                                                    = Left "argument to '-L' is missing"
optionParse ("-E":xs)                   (inf        ,outf     ,_    ,mem,fd,ld,ni,nl) = optionParse xs (     inf,outf,(1,1),mem,fd,ld,ni,nl)
optionParse ("-nostdinc":xs)            (inf        ,outf     ,_    ,mem,fd,ld,ni,nl) = optionParse xs (     inf,outf,(1,1),mem,fd,ld,ni,nl)
optionParse ("-nostdlib":xs)            (inf        ,outf     ,_    ,mem,fd,ld,ni,nl) = optionParse xs (     inf,outf,(1,1),mem,fd,ld,ni,nl)
optionParse (inf :xs)                   (_          ,outf     ,frmTo,mem,fd,ld,ni,nl) = optionParse xs (Just inf,outf,frmTo,mem,fd,ld,ni,nl)
optionParse []                          (Nothing    ,_        ,_    ,_  ,_ ,_ ,_ ,_ ) = Left "no input file"
optionParse []                          (Just infile,Just outf,(a,b),mem,fd,ld,ni,nl) = Right(infile,outf,(a,b),mem,fd,ld,ni,nl)
optionParse []                          (_          ,Nothing  ,_    ,_  ,_ ,_ ,_ ,_ ) = Left "no output file"