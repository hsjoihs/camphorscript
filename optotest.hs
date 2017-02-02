{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

import Camphor.SafePrelude
import Camphor.IO
import Camphor.Main2
import Camphor.Global.Synonyms
import Camphor.BF_interpreter
import Camphor.Show

data Info = String :-> String deriving(Show,Eq,Ord,Read)
unInfo :: Info -> (String,String)
unInfo(a :-> b) = (a,b)


main :: IO ()
main = do
 args <- getArgs
 optopatch args
 
info :: [String]
info = [
 "Usage: optotest <options to be passed to ccsc> <test file>" 
 ]
 
optopatch :: Options -> IO ()
optopatch [opts,tFile] = d4 (words opts) tFile
optopatch _         = mapM_ putStrLn info

d4 :: Options -> FilePath -> IO ()
d4 opts tFile = do
 (res,b,_) <- dispatch5 opts
 when (b /= 7 && b /= 8) $ abort "not compiled to BF"
 case res of 
  Nothing -> abort "parse error in ccsc"
  Just r -> case pureInterpreter r of 
   Left e -> outputErr e
   Right judgeFunc -> d5 judgeFunc tFile
	
d5 :: (String -> Maybe String) -> FilePath -> IO ()
d5 judgeFunc tFile = do
 is <- filter (not . ignored) . lines <$> getContentsFrom tFile
 let ins = mapM (liftE readMay) is -- this `mapM' is operated on `Either', not on `IO'.
 case ins of 
  Left e -> abort $ "the syntax of the following test case is invalid: "++e
  Right infos -> d6 judgeFunc $ map unInfo infos
 
d6 :: (Show a, Eq b) => (a -> Maybe b) -> [(a,b)] -> IO ()
d6 judgeFunc infos = do
 let res = forM infos $ \(i,o) -> liftE (\j -> do{o2 <- judgeFunc j; guard(o2 == o); return o} ) i
 case res of
  Left e -> abort $ "optotest failed when testing "++show e
  Right _ -> putStrLn "succeeded" >> return ()
   

 
scFirst :: String -> Bool
scFirst = (==";") . take 1 . dropWhile isSpace 

ignored :: String -> Bool
ignored str = scFirst str || all isSpace str