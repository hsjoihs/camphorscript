{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

import Camphor.SafePrelude
import Text.Parsec
import Camphor.Global.Operators    
import Camphor.Global.Synonyms
import Camphor.Step.Step1
import Camphor.Step.Step2
import Camphor.Step.Step3_I
import Camphor.Step.Step3_II
import Camphor.Step.Step5
import Camphor.Step.Step6
import Camphor.Step.Step7
import Camphor.Step.Step8
import Camphor.IO
import Camphor.Lib
import Camphor.CmdOptions
import Data.List(isPrefixOf)
import qualified Data.Map as M
import Control.Monad 
import Camphor.NonEmpty
import Camphor.Version

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
 
dispatch4 :: Options -> IO ()
dispatch4 [] = mapM_ putStrLn info
dispatch4 xs = case catMaybes $ map toDoubleOption xs of
 (o:os) -> do -- double option overwrites everything
  case last' (o:|os) of
   Version -> putStrLn $ "CHAtsFtD CamphorScript Compiler, version "++version_num
 []     -> case optionParse xs (S Nothing Nothing (4,8) Nothing [] [] False False M.empty) of
  Left e -> abort e
  Right (infile,outf,(a,b),mem,fds,lds,ni,nl,t) -> do
   let file_dir = fst $ splitFileName infile
   contents  <- getContentsFrom infile
   includer  <- getManyLibs (if nl then lds else  lib_dir:lds)
   includer2 <- getManyLibs (if ni then fds else file_dir:fds)
   outputParsed outf (fromTo' a b (step infile mem (includer,includer2,t)) contents) 

step :: FilePath -> Maybe MemSize -> Includers -> [Txt -> Either ParseError Txt]   
step file mem includers  = map ($file) [step1 includers,step2,step3_I,step3_II mem,step5,step6,step7,step8]

-- starts with xth(1-indexed) and ends with yth(1-indexed)
fromTo' :: Monad m => Int -> Int -> [a -> m a] -> a -> m a
fromTo' x y xs
 | x<1       = abort("step " ++ showNum x ++ "does not exist")
 | y>8       = abort("step " ++ showNum y ++ "does not exist")
 | otherwise = case (drop(x-1)$take y xs) of 
  (t:ts) -> foldl (>=>) t ts
  _      -> abort "first number of option -C must not be larger than the second"