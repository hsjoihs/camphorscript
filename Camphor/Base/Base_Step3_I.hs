{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts , NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind -fno-warn-unused-imports #-}
module Camphor.Base.Base_Step3_I
(step3_I
)where
import Camphor.SafePrelude
import Camphor.Global.Parsers
import Camphor.Global.Synonyms
import Text.Parsec hiding(token,State)
import Camphor.Transformer
{- 
	For now, it will delete lines and automatically adds indentation. 
	As character literals are already processed in Base_Step2,
	indentation can simply be done by counting '{'s and '}'s.
-}
step3_I :: FilePath -> Txt -> Either ParseError Txt
step3_I file str = do
 lns <- runParser parser3 (0,[]) (file ++ "--step3") (str <+> "\n")
 return $ mconcat lns

type Tabnum = Integer 
type Stat = (Tabnum,[FilePath])
newtype Comm = Comm String
type Data = [Between String Comm] 

parser3 :: Stream s m Char => ParsecT s Stat m [Txt]
parser3 = many line <* eof

 
line :: Stream s m Char => ParsecT s Stat m Txt 
line = do
 res <- other'
 stt@(n,r) <- getState
 let (ans,newS) = runState(process res) stt
 putState newS
 let ans' = dropWhile isSpace (unpack ans)
 let tabnum = case ans' of  '}':_ -> n-1; _     -> n
 let end = case r of [] -> "\n"; _ -> if blank res then "" else "\n"
 return $ end <+> genericReplicate tabnum '\t' <+> ans'

blank :: Data -> Bool
blank [] = True
blank (East x:xs)
 | all isSpace x = blank xs
 | otherwise     = False
blank (West _:xs) = blank xs 
 

process :: Data -> StateT Stat Identity Txt
process [] = return (pack "")
process (East x:xs) = do
 rest <- process xs
 modifyFst(genericLength(filter(=='{')x) - genericLength(filter(=='}')x)+)
 return $ x <+> rest
  
process (West(Comm c):xs) = case words c of 
 ["#","LINE","start",path,"#"] -> process xs <* modifySnd(path:)
 ["#","LINE","end"  ,_   ,"#"] -> process xs <* modifySnd(drop 1)
 _                             -> ("/*" <+> c <+> "*/")  <++$> process xs

 
other' :: Stream s m Char => ParsecT s u m Data
other' = do
 xs <- many(noneOf "\n/")
 do{newline;return [East xs]} <|> do{
  char '/';
  do{char '*';ys <- manyTill anyChar(try(string "*/"));zs <- other'; return([East xs,West $ Comm ys] ++ zs)} <|>
  do{ys <- other'; return $ (East $ xs++"/"):ys}
  } 
 
