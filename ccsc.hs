{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

import Camphor.SafePrelude
import Camphor.NonEmpty
import Camphor.Version
import Camphor.IO
import Camphor.Lib
import Camphor.CmdOptions
import Camphor.Global.Synonyms
import Camphor.Base.Base_Step1
import Camphor.Base.Base_Step2.Base_Step2
import Camphor.Base.Base_Step3_I
import Camphor.Base.Base_Step3_II
import Camphor.Base.Base_Step5
import Camphor.Base.Base_Step6
import Camphor.Base.Base_Step7
import Camphor.Base.Base_Step8
import Camphor.BF_interpreter
import Data.List(isPrefixOf)
import qualified Data.Map as M
import Control.Monad 

import Text.Parsec

getLibs :: FilePath -> IO FileToTxt 
getLibs dir = do
 conts'    <- getDirectoryContents dir  
 libs <- filterM (doesFileExist . snd) [ (file,dir </> file) | file <- conts', not("." `isPrefixOf` file)]
 let libs1 = map fst libs; libs2 = map snd libs
 texts <- mapM getContentsFrom libs2
 return (M.fromList $ zip libs1 (zip libs2 texts))

getManyLibs :: [FilePath] -> IO FileToTxt
getManyLibs dirs = do
 libs's <- mapM getLibs dirs 
 return(M.unions libs's)
 
main :: IO()
main = do
 args <- getArgs
 dispatch4 args

toDoubleOption :: String -> Maybe Overwriter
toDoubleOption "--version" = Just Version
toDoubleOption _           = Nothing
 
defaultStat :: Stat 
defaultStat = S{
 inputFile    = Nothing,     
 outputFile   = Nothing,
 fromTo       = (1,8),
 memSize      = Nothing,
 includeDirs  = [],
 libraryDirs  = [],
 noStdInc     = False,
 noStdLib     = False,
 macroTable   = M.empty,
 incDecMerge  = False,
 incDecToMult = False,
 memoryShred  = False,
 optim        = Naive,
 warnTill     = Nothing,
 run          = False
 }
 
dispatch4 :: Options -> IO ()
dispatch4 [] = mapM_ putStrLn info
dispatch4 xs = case mapMaybe toDoubleOption xs of
 (o:os) -> case last' (o:|os) of -- double option overwrites everything
  Version -> putStrLn $ "CHAtsFtD CamphorScript Compiler, version " ++ version_num
 []     -> case optionParse xs defaultStat of
  Left e -> abort e
  Right (infile,outf,(a,b),mem,fds,lds,ni,nl,t,_merge,_mult,_shred,_optim,_warnlev,rn) -> do
   let file_dir = fst $ splitFileName infile
   contents  <- getContentsFrom infile
   includer  <- getManyLibs (if nl then lds else  lib_dir:lds)
   includer2 <- getManyLibs (if ni then fds else file_dir:fds)
   res <- outputParsed outf (fromTo' a b (step infile mem (includer,includer2,t)) contents) 
   when (rn && ( b == 7 || b == 8)) $ case res of Nothing -> return(); Just r -> interpreterIO r

step :: FilePath -> Maybe MemSize -> Includers -> [Txt -> Either ParseError Txt]   
step file mem includers  = map ($file) [step1 includers,step2,step3_I,step3_II mem,step5,step6,step7,step8]

-- starts with xth(1-indexed) and ends with yth(1-indexed)
fromTo' :: Monad m => Int -> Int -> [a -> m a] -> a -> m a
fromTo' x y xs
 | x<1       = abort("step " ++ showNum x ++ "does not exist")
 | y>8       = abort("step " ++ showNum y ++ "does not exist")
 | otherwise = case drop(x-1)$take y xs of 
  (t:ts) -> foldl (>=>) t ts
  _      -> abort "first number of option -C must not be larger than the second"