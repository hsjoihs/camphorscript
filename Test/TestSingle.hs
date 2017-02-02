{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

module Test.TestSingle
(testSingle,info
)where
import Camphor.SafePrelude
import Camphor.IO
import Camphor.Main2
import Camphor.Global.Synonyms
import Camphor.BF_interpreter
import Camphor.Show

data Info = String :-> String deriving(Show,Eq,Ord,Read)
unInfo :: Info -> (String,String)
unInfo(a :-> b) = (a,b)

info :: [String]
info = [
 "Usage: optotest --single <options to be passed to ccsc> <test file>" 
 ]

testSingle :: Options -> IO ()
testSingle [opts,tFile] = d4 (words opts) tFile
testSingle _            = mapM_ putStrLn info

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
 let res = do{
  infos <- liftErr invalidCase $ mapM (liftE readMay) is;
  _     <- liftErr failed$forM (map unInfo infos) $ \(i,o) -> liftE (\j -> do{o2 <- judgeFunc j; guard(o2 == o); return o} ) i;
  return ()
  }
 case res of Left e -> abort e; Right _ -> putStrLn "succeeded" >> return ()


 
invalidCase :: String -> String
invalidCase = ("the syntax of the following test case is invalid: "++)

failed :: String -> String
failed e = "optotest failed when testing " ++ show e
   
liftErr :: (e -> e') -> Either e a -> Either e' a
liftErr f (Left  e) = Left$f e   
liftErr _ (Right a) = Right a 
   
scFirst :: String -> Bool
scFirst = (==";") . take 1 . dropWhile isSpace 

ignored :: String -> Bool
ignored str = scFirst str || all isSpace str