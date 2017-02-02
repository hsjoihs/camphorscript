{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{- Desugaring -}
module Camphor.Base_Step3_II
(step3_II
,parser3
,convert3

) where

import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Global
import Text.Parsec hiding(token)
import Control.Applicative hiding ((<|>),many)
import Camphor.NonEmpty
import qualified Data.Map as M
import Text.Parsec.Error
import Text.Parsec.Pos

data Com3_top=DEF|DEL|REA|WRI|COM|NUL deriving(Show)
data Com3_mid=ADD|SUB deriving(Show)
data Com3_bot=WHI|BLO deriving(Show)

data OneOf a b c = Null | Top a | Mid b | Bot c deriving (Show)

data Tree b c d f = Ns [OneOf d f(b,c,Tree b c d f)] deriving(Show)
type Node b c d f =        OneOf d f(b,c,Tree b c d f)

type Set3 = Node Com3_bot Ident (Com3_top,String) (Com3_mid,[Char],[Char])

step3_II :: Maybe MemSize -> FilePath -> Txt -> Either ParseError Txt
step3_II mem file str = do
 sets <- parse parser3 (file++"--step3_II") str
 convert3 mem file sets
 
parser3 :: Stream s m Char => ParsecT s u m [Set3]
parser3 = many sentences3

sentences3 :: Stream s m Char => ParsecT s u m Set3
sentences3 = def <|> del <|> add <|> sub <|> while <|> block <|> read_ <|> write <|> nul <|> emp <|> comm 
 where 
  def   = try(do{string"char"  ;spaces';xs<-identifier;spaces'; char ';';return$ Top(DEF,xs)})
  del   = try(do{string"delete";spaces';xs<-identifier;spaces'; char ';';return$ Top(DEL,xs)})
  add   = try(do{xs<-identifier;spaces';char '+';spaces';char '=';spaces'; ys<-byte;spaces';char ';';return$ Mid(ADD,xs,ys)})
  sub   = try(do{xs<-identifier;spaces';char '-';spaces';char '=';spaces'; ys<-byte;spaces';char ';';return$ Mid(SUB,xs,ys)})
  read_ = try(do{string "read" ;spaces';char '(';spaces';xs<-identifier;spaces';char ')';spaces';char ';';return$ Top(REA,xs)})
  write = try(do{string "write";spaces';char '(';spaces';xs<-identifier;spaces';char ')';spaces';char ';';return$ Top(WRI,xs)})
  nul   = try(do{sp<-many1 space;return$ Top(NUL,sp)})
  emp   =     do{char ';';return Null}
  comm  = try(do{string "/*";xs<-many(noneOf "*");string "*/";return$Top(COM,"/*"++xs++"*/")})

while :: Stream s m Char => ParsecT s u m Set3  
while = try(do{
 string "while"; spaces'; char '('; spaces'; xs<-identifier; spaces'; char ')';
 spaces'; char '{';
 spaces'; ks<-parser3; spaces';
 char '}';
 return$Bot(WHI,xs,Ns ks)})
 
block :: Stream s m Char => ParsecT s u m Set3 
block = try(do{
 char '{';
 spaces'; ks<-parser3; spaces';
 char '}';
 return$Bot(BLO,"",Ns ks)})


type VarNum = Integer
type Table3 = M.Map Ident VarNum -- variable, variable num
type CurrState = (Int,NonEmpty Table3,[VarNum]) -- block num, defined variables(inner scope first), used variable num
{- Table3 must not be empty -}



convert3 :: Maybe MemSize -> FilePath -> [Set3] -> Either ParseError Txt
convert3 mem file xs = snd <$> convert3' mem file ((1,M.empty :| [],[]),xs)

minUnused :: Maybe MemSize -> [VarNum] -> FilePath -> Either ParseError VarNum
minUnused Nothing = minUnused' [0..]
minUnused(Just x) = minUnused' [0..(x-1)] 

minUnused' :: [Integer] -> [VarNum] -> FilePath -> Either ParseError VarNum
minUnused' list xs f= let filtered = filter(`notElem` xs) list in
 case filtered of 
  [] -> Left$newErrorMessage (Message$ "memory ran out")(newPos (f++"--step3_II'") 0 0)
  x:_ -> Right x
  
remove :: VarNum->[VarNum]->[VarNum]
remove x xs = filter (/=x) xs

lookup'::Ord k=>k->[M.Map k a]->Maybe a -- lookup towards the outer scope until you find a variable
lookup' _ []     = Nothing
lookup' i (t:ts) = case(M.lookup i t)of Just a->Just a; Nothing-> lookup' i ts

convert3' :: Maybe MemSize -> FilePath -> (CurrState,[Set3]) -> Either ParseError (Table3,String) -- variables left undeleted 


convert3' _ _((_ ,s:|_  ,_ ),[]                    ) = Right (s,"")


convert3' m f((n ,s:|st ,ls),(Top(DEF,ide     ):xs)) 
 | isJust(M.lookup ide s)                        = Left $newErrorMessage (Message$"identifier "++show ide++"is already defined")(newPos (f++"--step3_II'") 0 0)
 | otherwise                                     = do
  new <- minUnused m ls f
  convert3' m f((n, M.insert ide new s :| st,new:ls),xs)


convert3' m f((n ,s:|st ,ls),(Top(DEL,ide     ):xs)) = case (M.lookup ide s) of
   Just  k                                      -> convert3' m f((n, M.delete ide s :| st,remove k ls),xs)
   Nothing                                      -> Left $newErrorMessage (Message$"identifier "++show ide++"is not defined or is already deleted in this scope")(newPos (f++"--step3_II'") 0 0)

convert3' m f(state         ,(Top(NUL,sp      ):xs)) = (sp++) <$$> convert3' m f(state,xs) 


convert3' m f((n ,st    ,ls),(Mid(ADD,ide,  nm):xs)) = case (lookup' ide (toList st)) of
   Just  k                                      -> (\x->"mov "++show k++"; inc "++nm++"; "++x) <$$> convert3' m  f((n,st,ls),xs)
   Nothing                                      -> Left $newErrorMessage (Message$"identifier "++show ide++"is not defined")(newPos (f++"--step3_II'") 0 0)
   
convert3' m f((n ,st    ,ls),(Mid(SUB,ide,  nm):xs)) = case (lookup' ide (toList st)) of
   Just  k                                      -> (\x->"mov "++show k++"; dec "++nm++"; "++x) <$$> convert3' m  f((n,st,ls),xs)
   Nothing                                      -> Left $newErrorMessage (Message$"identifier "++show ide++"is not defined")(newPos (f++"--step3_II'") 0 0)

convert3' m f((n ,st    ,ls),(Top(REA,ide     ):xs)) = case (lookup' ide (toList st)) of
   Just  k                                      -> (\x->"mov "++show k++"; _input; "++x) <$$> convert3'  m f((n,st,ls),xs)
   Nothing                                      -> Left $newErrorMessage (Message$"identifier "++show ide++"is not defined")(newPos (f++"--step3_II'") 0 0)

convert3' m f((n ,st    ,ls),(Top(WRI,ide     ):xs)) = case (lookup' ide (toList st)) of
   Just  k                                      -> (\x->"mov "++show k++"; output; "++x) <$$> convert3'  m f((n,st,ls),xs)
   Nothing                                      -> Left $newErrorMessage (Message$"identifier "++show ide++"is not defined")(newPos (f++"--step3_II'") 0 0)

convert3' m f(state         ,(Null             :xs)) = (' ':) <$$> convert3' m f(state,xs)
convert3' m f(state         ,(Top(COM,cm)      :xs)) = (cm++) <$$> convert3' m f(state,xs)

convert3' m f((n ,st    ,ls),(Bot(WHI,ide,Ns v):xs)) = case (lookup' ide (toList st)) of
   Just k                                       -> do
    (table1,res1) <- convert3'  m f((n+1,M.empty `cons` st,ls),v ) -- inside the loop
    if not(M.null table1) 
     then let leftList = map fst $ M.toList table1 in 
     Left $ newErrorMessage (Message$"identifier"++message leftList++" not deleted")(newPos (f++"--step3_II'") 0 0) 
     else do
    (table2,res2) <- convert3'  m f((n  ,st               ,ls),xs) -- left
    return $ (table2,"mov " ++ show k ++ "; loop; " ++ res1 ++ "mov " ++ show k ++ "; pool; " ++ res2)
   Nothing                                      -> Left $newErrorMessage (Message$"identifier "++show ide++"is not defined")(newPos (f++"--step3_II'") 0 0)
   
convert3' m f((n ,st    ,ls),(Bot(BLO,_  ,Ns v):xs)) =  do
    (table1,res1) <- convert3'  m f((n+1,M.empty `cons` st,ls),v ) -- inside the loop
    if not(M.null table1) 
     then let leftList = map fst $ M.toList table1 in 
     Left $ newErrorMessage (Message$"identifier"++message leftList++" not deleted")(newPos (f++"--step3_II'") 0 0) 
     else do
    (table2,res2) <- convert3'  m f((n  ,st               ,ls),xs) -- left
    return $ (table2,res1 ++ res2)

message :: [String] -> String
message qs = case qs of 
 [] -> "" 
 [q] -> " "++q++" is"
 rs -> "s "++show rs++" are" 