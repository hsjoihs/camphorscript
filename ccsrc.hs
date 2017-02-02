{-# OPTIONS -Wall #-}

import System.Environment
import Control.Monad
import Text.Parsec
import Camphor.Global.Synonyms
import Camphor.IO
import CamphorR.R_Step8
import CamphorR.R_Step7
import CamphorR.R_Step4
import CamphorR.CmdOptions

step :: FilePath -> [String -> Either ParseError String]
step file=[step8_R,step7_R file,Right,Right,step4_R]

fromTo' :: Monad m => Int -> Int -> [a -> m a] -> a -> m a
fromTo' x y xs
 | x<y       = abort "first number of option -C must not be smaller than the second"
 | x>8       = abort$"step "++show x++"does not exist"
 | y<1       = abort$"step "++show y++"does not exist"
 | otherwise = foldl1 (>=>)(drop (8-x) $ take(9-y) $ xs)

main :: IO()
main = do
 args <- getArgs
 dispatch4 args

dispatch4 :: Options -> IO ()
dispatch4 []     = mapM_ putStrLn info
dispatch4 xs     = case optionParse xs (Nothing,Nothing,(8,8)) of
 Left e -> abort e
 Right (infile,outf,(a,b)) -> do
  contents <- getContentsFrom infile
  outputParsed outf (fromTo' a  b (step infile) contents)