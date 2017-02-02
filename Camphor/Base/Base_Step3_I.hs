{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts , NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind -fno-warn-unused-imports #-}
module Camphor.Base.Base_Step3_I
(step3_I
)where
import Camphor.SafePrelude
import Camphor.Global.Parsers
import Camphor.Global.Synonyms
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)
{- 
	For now, it will delete lines and automatically adds indentation. 
	As character literals are already processed in Base_Step2,
	indentation can simply be done by counting '{'s and '}'s.
-}
step3_I :: FilePath -> Txt -> Either ParseError Txt
step3_I file str = do
 lns <- runParser parser3 (0,[]) (file ++ "--step3") (str++"\n")
 return(concat lns)

type Tabnum = Integer 
type Stat = (Tabnum,[FilePath])
newtype Comm = Comm String

parser3 :: Stream s m Char => ParsecT s Stat m [Txt]
parser3 = do
 res <- many line
 eof
 return res
 
line :: Stream s m Char => ParsecT s Stat m Txt 
line = do
 res <- other'
 stt@(n,r) <- getState
 let (ans,newS) = process res stt
 putState newS
 let ans' = dropWhile isSpace ans
 let tabnum = case ans' of  '}':_ -> n-1; _     -> n
 let end = case r of [] -> "\n"; _ -> if blank res then "" else "\n"
 return (end ++ genericReplicate tabnum '\t' ++ ans' )

blank :: Data -> Bool
blank [] = True
blank (East x:xs)
 | all isSpace x = blank xs
 | otherwise     = False
blank (West _:xs) = blank xs 
 
-- I wanna use State 
process :: Data -> Stat -> (Txt,Stat)
process [] stt = ("",stt)
process (East x:xs) stt = (x++rest,(newN,r)) 
 where 
  (rest,(n,r)) = process xs stt
  newN = n + genericLength(filter(=='{')x) - genericLength(filter(=='}')x)
  
process (West(Comm c):xs) stt = case words c of 
 ["#","LINE","start",path,"#"] -> (rest,(n,r2))where (rest,(n,r)) = process xs stt; r2 = path:r 
 ["#","LINE","end"  ,_   ,"#"] -> (rest,(n,r2))where (rest,(n,r)) = process xs stt; r2 = drop 1 r 
 _                             -> ("/*"++c++"*/"++rest,(n,r)) where (rest,(n,r)) = process xs stt

 
other' :: Stream s m Char => ParsecT s u m Data
other' = do
 xs <- many(noneOf("\n/"))
 do{newline;return [East xs]} <|> do{
  char '/';
  do{char '*';ys <- manyTill anyChar(try(string "*/"));zs <- other'; return([East xs,West $ Comm ys] ++ zs)} <|>
  do{ys <- other'; return $ [East $ xs++"/"]++ys}
  } 
 
type Data = [Between Txt Comm] 