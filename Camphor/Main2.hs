{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}
module Camphor.Main2
(dispatch4
,dispatch5
)where
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
import Camphor.Base.Base_Step4.Base_Step4
import Camphor.Base.Base_Step5
import Camphor.Base.Base_Step6
import Camphor.Base.Base_Step7
import Camphor.Base.Base_Step8
import Camphor.BF_interpreter
import Camphor.Transformer
import Camphor.Warn
import qualified Data.Map as M
import Text.Parsec

getLibs :: FilePath -> IO FileToTxt 
getLibs dir = do
 libs <- getDirectoryFiles dir 
 let libs1 = map fst libs; libs2 = map snd libs
 texts <- mapM getContentsFrom libs2
 return (M.fromList $ zip libs1 (zip libs2 texts))

getManyLibs :: [FilePath] -> IO FileToTxt
getManyLibs dirs = do
 libs's <- mapM getLibs dirs 
 return(M.unions libs's)
 
toDoubleOption :: String -> Maybe Overwriter
toDoubleOption "--version" = Just Version
toDoubleOption "--help"    = Just Help
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
 warnTill     = Just Important,
 run          = False,
 warnNum      = Nothing
 }
 
dispatch4 :: Options -> IO ()
dispatch4 [] = outputInfo
dispatch4 xs = case mapMaybe toDoubleOption xs of
 (o:os) -> case last (o:|os) of -- double option overwrites everything
  Version -> putStrLn $ "CHAtsFtD CamphorScript Compiler, version " ++ version_num
  Help    -> outputInfo
 []     -> do
  (res,b,rn) <- dispatch5 xs
  when (rn && ( b == 7 || b == 8)) $ forM_ res interpreterIO -- res is Maybe
   
dispatch5 :: Options -> IO (Maybe Txt,Int,Bool)
dispatch5 xs = case optionParse xs defaultStat of
 Left e -> abort e
 Right S{inputFile  = Nothing} -> abort "no input file"
 Right S{outputFile = Nothing} -> abort "no output file"
 Right stat@S{inputFile = Just infile, outputFile = Just outf, fromTo = (a,b)} -> do 
  let fds = includeDirs stat; lds = libraryDirs stat; wnum = warnNum stat; idm = incDecMerge stat  -- results
  let file_dir = fst $ splitFileName infile
  let to4 = idm
  contents  <- getContentsFrom infile
  includer  <- getManyLibs (if noStdLib stat then lds else  lib_dir:lds)
  includer2 <- getManyLibs (if noStdInc stat then fds else file_dir:fds)
  let result = runWriterT $ fromTo' a b (step infile (memSize stat) (includer,includer2,macroTable stat) to4) contents
  let (parsed,warns') = (fst <$> result, snd <$> result)
  case (warnTill stat,warns') of (Just lv, Right warns) -> outputWarn wnum lv warns; _ -> return ()
  res <- outputParsed outf parsed
  return(res,b,run stat)  



lft :: (Monoid s,Monad m) => (a -> b -> m c) -> a -> b -> WriterT s m c
lft f a b = lift(f a b)
   
step :: FilePath -> Maybe MemSize -> Includers -> To4 -> [  Txt -> WriterT Warnings (Either ParseError) Txt  ]   
step file mem includers to4 = map ($file) [step1 includers,step2,lft$step3_I,lft$step4 to4,lft$step5 mem,lft$step6,lft step7,lft$step8]

-- starts with xth(1-indexed) and ends with yth(1-indexed)
fromTo' :: Monad m => Int -> Int -> [a -> m a] -> a -> m a
fromTo' x y xs
 | x<1       = abort("step " ++ showNum x ++ " does not exist")
 | y>8       = abort("step " ++ showNum y ++ " does not exist")
 | otherwise = case drop(x-1)$take y xs of 
  (t:ts) -> foldl (>=>) t ts
  _      -> abort "first number of option -C must not be larger than the second"