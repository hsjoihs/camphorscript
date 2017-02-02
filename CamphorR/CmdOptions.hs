{-# OPTIONS -Wall #-}
module CamphorR.CmdOptions
(info,optionParse
)where
import Camphor.Global.Synonyms

info :: [String]
info = [
 "CHAtsFtD CamphorScript Reverse Compiler - Copyright (c) 2014- CHAtsFtD ",
 "Usage: ccsrc [options] [-o outfilepath] infile",
 "options: ",
 "-Cnum[num]  reverse compile from step 'num' to step 'num'"
 ]
type Stat = (Maybe FilePath,Maybe FilePath,(Int,Int)) -- in,out,from,to 
optionParse :: Options -> Stat -> Either String (FilePath,FilePath,(Int,Int))
optionParse ("-o":outf:xs)    (inf    ,_      ,frmTo) = optionParse xs (inf,Just outf,frmTo)
optionParse ["-o"]             _                            = Left "argument to '-o' is missing"
optionParse (['-','C',x,y]:xs)(inf    ,outf   ,_    ) = optionParse xs (inf,outf      ,(read[x],read[y]))
optionParse (['-','C',x]  :xs)(inf    ,outf   ,_    ) = optionParse xs (inf,outf      ,(read[x],read[x]))
optionParse (inf:xs)          (_      ,outf   ,frmTo) = optionParse xs (Just inf,outf      ,frmTo)
optionParse []                (Just i ,Just o ,(a,b)) = Right(i,o,(a,b))

optionParse []                (Nothing,_      ,_    )  = Left "no input files"
optionParse []                (_      ,Nothing,_    )  = Left "no output files"