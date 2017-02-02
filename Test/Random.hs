{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}

module Test.Random
(makeRand
)where
import Camphor.SafePrelude
import System.Random 
import Data.Word (Word8)
import Test.TestSingle
import Camphor.Global.Synonyms
import Camphor.Show

randsIO :: IO [Word8]
randsIO = randoms <$> newStdGen

makeRand :: Options -> Integer -> FilePath -> IO ()
makeRand opts num tFile = do
 judgeFunc <- getJudgeFunc opts
 dat <- map toStr <$> getRandOf num
 let test = makeTest dat judgeFunc
 putStrLn "starting"
 writeFile tFile test--writeFile tFile test
 putStrLn "finished"

 
toStr :: [Word8] -> String
toStr = map $ chr . fromIntegral 
 
getRandOf :: Integer -> IO [[Word8]]
getRandOf num = (take 44 . groupNum num) <$> randsIO 
 
groupNum :: Integer -> [a] -> [[a]] 
groupNum n xs
 | null $ genericTake n xs = []
 | otherwise               = genericTake n xs : groupNum n(genericDrop n xs)

makeTest :: [String] -> (String -> Maybe String) -> String 
makeTest dat judgeFunc = unlines$map func dat
 where 
  func x = case judgeFunc x of
   Nothing -> show(FAIL x)
   Just r -> show(x :-> r)