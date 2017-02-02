{-# OPTIONS -Wall #-}
module CamphorR.CmdOptions
(info,optionParse,emptyStat,Stat()
)where
import Camphor.Global.Synonyms

info :: [String]
info = [
 "CHAtsFtD CamphorScript Reverse Compiler - Copyright (c) 2014- CHAtsFtD ",
 "Usage: ccsrc [options] [-o outfilepath] infile",
 "options: ",
 "-Cnum[num]  reverse compile from step 'num' to step 'num'",
 "-f0.6       reverse compile like 0.6"
 ]

emptyStat :: Stat
emptyStat = Stat Nothing Nothing 8 8
 
data Stat = Stat{ inp :: Maybe FilePath, outp :: Maybe FilePath, from :: Int, to :: Int} deriving(Show)
optionParse :: Options -> Stat -> Either String (FilePath,FilePath,(Int,Int))
optionParse ("-o":outf:xs)     s = optionParse xs s{outp = Just outf}
optionParse ["-o"]             _ = Left "argument to '-o' is missing"
optionParse (['-','C',x,y]:xs) s = optionParse xs s{from = read[x], to = read[y]}
optionParse (['-','C',x]  :xs) s = optionParse xs s{from = read[x], to = read[x]}
optionParse ("-f0.6":xs)       s = optionParse xs s
optionParse (inf:xs)           s = optionParse xs s{inp = Just inf}
optionParse []                (Stat(Just i)(Just o)a b) = Right(i,o,(a,b))
optionParse []                (Stat Nothing _ _ _)  = Left "no input files"
optionParse []                (Stat _ Nothing _ _)  = Left "no output files"