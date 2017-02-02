{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module CamphorTest.TestSingle
(testSingle,info,testSingle',getJudgeFunc,Info(..) 
)where
import Camphor.SafePrelude
import Camphor.IO
import Camphor.Main2
import Camphor.Global.Synonyms
import Camphor.BF_interpreter
import Camphor.Show

data Info = String :-> String | FAIL String deriving(Show,Eq,Ord,Read)

info :: [String]
info = [
 "Usage: ",
 "    optotest --single <options to ccsc> <test file>   tests single file ", 
 "    optotest --getver                                 gets version from directory name", 
 "    optotest --make <file name> n                     makes test file from `file name' whose test length is `n' ", 
 "    optotest                                          test all" 
 
 ]

testSingle :: Options -> IO ()
testSingle [opts,tFile] = testSingle'  opts tFile
testSingle _            = mapM_ putStrLn info

testSingle' :: String -> FilePath -> IO ()
testSingle' opts tFile = d4 (words opts) tFile

getJudgeFunc :: Options -> IO (String -> Maybe String)
getJudgeFunc opts = do
 (res,b,_) <- dispatch5 opts
 when (b /= 7 && b /= 8) $ abort "not compiled to BF"
 case res of 
  Nothing -> abort "parse error in ccsc"
  Just r -> case pureInterpreter r of 
   Left e -> abort$"parse error at " ++ show e
   Right judgeFunc -> return judgeFunc


d4 :: Options -> FilePath -> IO ()
d4 opts tFile = do
 judgeFunc <- getJudgeFunc opts
 is <- filter (not . ignored) . lines <$> getContentsFrom tFile 
 mapM_ (\i -> putStr' "." >> eitherToIO(test judgeFunc i)) is
 putStrLn "all succeeded" 

test :: (String -> Maybe String) -> String -> Either String ()
test judgeFunc inf = do
 io <- liftErr invalidCase (liftE readMay inf);
 case io of
  i :-> o -> voidM $ liftErr failed $ liftE (\j -> do{o2 <- judgeFunc j; guard(o2 == o); return o}) i;
  FAIL i  -> maybe (return ()) succeeded $ judgeFunc i ; 


eitherToIO :: Either String b -> IO ()
eitherToIO (Left e) = abort e
eitherToIO _        = return () 
 
invalidCase :: String -> String
invalidCase = ("the syntax of the following test case is invalid: "++)

failed :: String -> String
failed e = "optotest failed when testing " ++ showStr e

succeeded :: String -> Either String a
succeeded i = Left$"optotest succeeded when testing " ++ showStr i
   
liftErr :: (e -> e') -> Either e a -> Either e' a
liftErr f (Left  e) = Left$f e   
liftErr _ (Right a) = Right a 
   
scFirst :: String -> Bool
scFirst = (==";") . take 1 . dropWhile isSpace 

ignored :: String -> Bool
ignored str = scFirst str || all isSpace str