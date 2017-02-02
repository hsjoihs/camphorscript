{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

import Camphor.SafePrelude
import Text.Parsec
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
import Camphor.CmdOptions
import Data.List(isPrefixOf)
import qualified Data.Map as M
import Data.Maybe
import Control.Monad

getLibs :: FilePath -> IO FileToTxt 
getLibs dir = do
  conts' <- getDirectoryContents dir
  let conts = [ file | file <- conts', not("." `isPrefixOf` file)]
  libs' <- forM conts $ \f -> do{e <- doesFileExist (dir </> f); if e then return(Just f) else return(Nothing)}
  let libs = catMaybes libs'
  texts <- mapM getContentsFrom (map (dir </>) libs)
  return (M.fromList $ zip libs texts)

main :: IO()
main = do
 args <- getArgs
 dispatch4 args
 
dispatch4 :: Options -> IO ()
dispatch4 [] = mapM_ putStrLn info
dispatch4 xs = case optionParse xs (Nothing,Nothing,(4,8),Nothing) of
 Left e -> abort e
 Right (infile,outf,a,b,mem) -> do
  let file_dir = fst $ splitFileName infile
  contents <- getContentsFrom infile
  includer <- getLibs lib_dir
  includer2 <- getLibs file_dir
  outputParsed outf (fromTo' a b (step infile (includer,includer2,file_dir) mem) contents) 

step :: FilePath -> Includers -> Maybe MemSize -> [Txt -> Either ParseError Txt]   
step file includers mem = map ($file) [step1 includers,step2,step3_I,step3_II mem,step5,step6,step7,step8]

-- starts with xth(1-indexed) and ends with yth(1-indexed)
fromTo' :: Monad m => Int -> Int -> [a -> m a] -> a -> m a
fromTo' x y xs
 | x<1       = abort("step "++show x++"does not exist")
 | y>8       = abort("step "++show y++"does not exist")
 | otherwise = case (drop(x-1)$take y xs) of 
  (t:ts) -> foldl (>=>) t ts
  _      -> abort "first number of option -C must not be larger than the second"