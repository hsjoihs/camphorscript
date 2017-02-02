{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.CmdOptions
(Stat(..),optionParse,info,Overwriter(..),Optim(..),outputInfo
)where
import Camphor.SafePrelude
import Camphor.Global.Synonyms
import qualified Data.Map as M
import Camphor.Warn
info :: [String]
info = [
 "CHAtsFtD CamphorScript Compiler - Copyright (c) 2014- CHAtsFtD ",
 "Usage: ccsc [options] [-o outfilepath] infile",
 "options: ",
 "-C<num>[<num>]  compile from step 'num' to step 'num'",
 "-E              C preprocess only",
 "-I<dir>         add include path 'dir'",
 "-L<dir>         add library path 'dir'",
 "-nostdinc       do not use standard include paths (current directory)",
 "-nostdlib       do not use standard library paths ",
 "-m num          limit the number of memory used in the compiled Brainf*ck",
 "con             use stdin or stdout instead of infile or outfile",
 "-D<macro>       define an empty 'macro'",
 "-D<macro>=<val> define a 'macro' with 'val' as its value",
 "--version       displays the version of the compiler",
 "-run            run compiled source",
 "-Wnone          output no warning",
 "-Wless          output less warning",
 "-Wnormal        output normal warning",
 "-Wmore          output more warning",
 "-Wall           output all warning",
 "-Wnum <num>     do not output more than 'num' warnings",
 "-f0.6           compile like 0.6",
 "nul             null device"
 ]
 
outputInfo :: IO ()
outputInfo = mapM_ putStrLn info

data Overwriter = Version | Help deriving(Show,Eq,Ord)
data Optim = Naive | HalfS deriving(Show,Eq,Ord)
data Stat = S {
 inputFile    :: Maybe FilePath, 
 outputFile   :: Maybe FilePath,
 fromTo       :: (Int,Int),
 memSize      :: Maybe MemSize,
 includeDirs  :: [FilePath],
 libraryDirs  :: [FilePath],
 noStdInc     :: Bool,
 noStdLib     :: Bool,
 macroTable   :: M.Map Ident String,
 incDecMerge  :: Bool,
 incDecToMult :: Bool,
 memoryShred  :: Bool,
 optim        :: Optim,
 warnTill     :: Maybe WarnLevel,
 run          :: Bool,
 warnNum      :: Maybe Int
 } 
 
optionParse :: Options -> Stat -> Either String Stat
optionParse ["-o"]                       _    =  Left "argument to '-o' is missing"
optionParse ["-m"]                       _    =  Left "argument to '-m' is missing"
optionParse ["-I"]                       _    =  Left "argument to '-I' is missing"
optionParse ["-L"]                       _    =  Left "argument to '-L' is missing"
optionParse ["-D"]                       _    =  Left "argument to '-D' is missing"
optionParse ["-Wnum"]                    _    =  Left "argument to '-Wnum' is missing"
optionParse ("-m":mem               :xs) stat =  case readMay mem of
 Just mem'                                    -> optionParse xs stat{memSize = Just mem'}
 Nothing                                      -> Left "argument to '-m' is not an integer"
optionParse ("-Wnum":n              :xs) stat =  case readMay n of 
 Just m                                       -> optionParse xs stat{warnNum = Just m}
 Nothing                                      -> Left "argument to '-Wnum' is not an integer"

optionParse (o@['-','C',x,y]        :xs) stat =  case (readMay[x],readMay[y]) of
 (Just x',Just y')                            -> optionParse xs stat{fromTo = (x',y')}
 _                                            -> Left $ "incorrect format " ++ showStr o ++ " of option -Cnum[num]" 
optionParse (p@['-','C',x]          :xs) stat = case readMay [x] of 
 Just x'                                      -> optionParse xs stat{fromTo = (x',x')}
 Nothing                                      -> Left $ "incorrect format " ++ showStr p ++ " of option -Cnum[num]"
optionParse (('-':'o':(outf@(_:_))) :xs) stat =  optionParse xs stat{outputFile = Just outf}
optionParse ("-o":outf              :xs) stat =  optionParse xs stat{outputFile = Just outf}
optionParse (('-':'I':(dir@(_:_)))  :xs) stat =  optionParse xs stat{includeDirs = includeDirs stat ++ [dir]}
optionParse ("-I":dir               :xs) stat =  optionParse xs stat{includeDirs = includeDirs stat ++ [dir]}
optionParse (('-':'L':(dir@(_:_)))  :xs) stat =  optionParse xs stat{libraryDirs = libraryDirs stat ++ [dir]}
optionParse ("-L":dir               :xs) stat =  optionParse xs stat{libraryDirs = libraryDirs stat ++ [dir]}
optionParse (('-':'D':(def@(_:_)))  :xs) stat = case break (=='=') def of
 (idn,"")                                     -> optionParse xs stat{macroTable = M.insert idn "" $ macroTable stat}
 (idn,_:rp)                                   -> optionParse xs stat{macroTable = M.insert idn rp $ macroTable stat}
 -- optionParse xs stat{noStdLib = True}
optionParse ("-E"                   :xs) stat =  optionParse xs stat{fromTo = (1,1)}
optionParse ("-nostdinc"            :xs) stat =  optionParse xs stat{noStdInc = True}
optionParse ("-nostdlib"            :xs) stat =  optionParse xs stat{noStdLib = True}
optionParse ("-fincdecmerge"        :xs) stat =  optionParse xs stat{incDecMerge = True}
optionParse ("-fno-incdecmerge"     :xs) stat =  optionParse xs stat{incDecMerge = False}
optionParse ("-fincdectomult"       :xs) stat =  optionParse xs stat{incDecToMult = True}
optionParse ("-fno-incdectomult"    :xs) stat =  optionParse xs stat{incDecToMult = False}
optionParse ("-fmemoryshred"        :xs) stat =  optionParse xs stat{memoryShred = True}
optionParse ("-fno-memoryshred"     :xs) stat =  optionParse xs stat{memoryShred = False}
optionParse ("-fnaiverecycle"       :xs) stat =  optionParse xs stat{optim = Naive}
optionParse ("-fhalfscramble"       :xs) stat =  optionParse xs stat{optim = HalfS}
optionParse ("-f0.6"                :xs) stat =  optionParse xs stat{incDecMerge = False, incDecToMult = False, memoryShred = False, optim = Naive, warnTill = Nothing, run = False}
optionParse ("-Wnone"               :xs) stat =  optionParse xs stat{warnTill = Nothing}
optionParse ("-Wless"               :xs) stat =  optionParse xs stat{warnTill = Just Crucial}
optionParse ("-Wnormal"             :xs) stat =  optionParse xs stat{warnTill = Just Important}
optionParse ("-Wmore"               :xs) stat =  optionParse xs stat{warnTill = Just Helpful}
optionParse ("-Wall"                :xs) stat =  optionParse xs stat{warnTill = Just Verbose}


optionParse ("-run"                 :xs) stat =  optionParse xs stat{run = True, outputFile = Just $ fromMaybe "nul" $ outputFile stat}
optionParse (o@('-':_)              :_ ) _    =  
 Left $ "unknown option " ++ showStr o ++ ": use " ++ showStr("./" ++ o) ++ " to compile a file named " ++ showStr o 
optionParse (inf                    :xs) stat =  optionParse xs stat{inputFile = Just inf}
optionParse []                           stat = Right stat