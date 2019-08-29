{-# LANGUAGE FlexibleContexts ,NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{- Desugaring -}
module Camphor.Base.Base_Step5
(step5
-- ,parser5
-- ,convert5
) where

import Camphor.SafePrelude 
import Camphor.Show
import Camphor.NonEmpty
import Camphor.Global.Parsers
import Camphor.Global.Utilities
import Camphor.Global.Synonyms
import Text.Parsec hiding(token)
import qualified Data.Map as M

data Com5_top = DEF String | DEL String | REA String | WRI String | COM String | NUL String | AS0 String | ADD String String | SUB String String deriving(Show)
data Com5_bot = WHI Ident | BLO deriving(Show)

data OneOf a c = Null | Top a | Bot c deriving (Show)

newtype Tree b d = Ns [OneOf d (b,Tree b d)] deriving(Show)
type Node b d =     OneOf d (b,Tree b d)

type Set5 = Node Com5_bot Com5_top 

step5 :: Maybe MemSize -> FilePath -> Txt -> Either ParseError Txt
step5 mem file str = do
 sets <- parse parser5' (file ++ "--step5") str
 convert5 mem file sets
 
parser5' :: Stream s m Char => ParsecT s u m [Set5]
parser5' = parser5 <* eof

parId :: Stream s m Char => ParsecT s u m Ident
parId = do{spaces';char '(';spaces';xs <- identifier;spaces';char ')';spaces';return xs}

parIdSC :: Stream s m Char => ParsecT s u m Ident
parIdSC = parId <* char ';'

parser5 :: Stream s m Char => ParsecT s u m [Set5]
parser5 = concat <$> many sentences5


sentences5 :: Stream s m Char => ParsecT s u m [Set5]
sentences5 = def <|> del <|> asser <|> add <|> sub <|> while <|> block <|> read_ <|> write <|> nul <|> emp <|> comm 
 where 
  idSC  = do{xs <- identifier;spaces'; char ';';return xs}
  eqIdSC= do{spaces';char '=';spaces'; ys <- byte;spaces';char ';';return ys}
  def   = do{try(do{string "char"  ;spaces';});xs <- idSC;return[Top(DEF xs)]}
  del   = do{try(do{string "delete";spaces';});xs <- idSC;return[Top(DEL xs)]}
  asser = do{try(do{string "assert_zero";spaces';});xs <- idSC;return[Top(AS0 xs)]}
  add   = try(do{xs <- identifier ;spaces';char '+';ys <- eqIdSC;return[Top(ADD xs ys)]})
  sub   = try(do{xs <- identifier ;spaces';char '-';ys <- eqIdSC;return[Top(SUB xs ys)]})
  read_ = try(do{string "read"  ;xs <- parIdSC;return[Top(REA xs)]})
  write = try(do{string "write" ;xs <- parIdSC;return[Top(WRI xs)]})
  nul   = try(do{sp <- many1 space;return[Top(NUL sp)]})
  emp   =     do{char ';';return[Null]}
  comm  = try(do{string "/*";xs<-many(noneOf "*");string "*/";return[Top(COM$"/*"++xs++"*/")]})

curly :: Stream s m Char => ParsecT s u m [Set5]
curly = do{char '{';
 spaces'; ks <- parser5; spaces';
 char '}';return ks;}

while :: Stream s m Char => ParsecT s u m [Set5] 
while = try(do{
 string "while"; xs <- parId;
 spaces'; ks <- curly;
 return[Bot(WHI xs,Ns ks),Top(AS0 xs)]})
 
block :: Stream s m Char => ParsecT s u m [Set5]
block = try(do{
 ks <- curly;
 return[Bot(BLO,Ns ks)]})

-- addAssert :: [Set5] -> [Set5]

type Table5 = M.Map Ident Address -- variable, variable address
type CurrState = (Int,NonEmpty Table5,[Address]) -- block num, defined variables(inner scope first), used address
{- Table5 must not be empty -}

convert5 :: Maybe MemSize -> FilePath -> [Set5] -> Either ParseError Txt
convert5 mem file xs = snd <$> convert5' mem file ((1,M.empty :| [],[]),xs)

minUnused :: Maybe MemSize -> [Address] -> FilePath -> Either ParseError Address
minUnused Nothing = minUnused' [0..]
minUnused(Just x) = minUnused' [0..(x-1)] 

minUnused' :: [Address] -> [Address] -> FilePath -> Either ParseError Address
minUnused' list used f= let filtered = filter(`notElem` used) list in
 case filtered of 
  [] -> makeErr(Message "memory ran out")(f++"--step5'") 0 0
  x:_ -> Right x
  
msgIde :: Show a => a -> String -> Message
msgIde ide left= Message$"identifier "++show ide++left


lookup' :: Ord k => k -> [M.Map k a] -> Maybe a -- lookup towards the outer scope until you find a variable
lookup' _ []     = Nothing
lookup' i (t:ts) = case M.lookup i t of
 Just a  -> Just a
 Nothing -> lookup' i ts
 


convert5' :: Maybe MemSize -> FilePath -> (CurrState,[Set5]) -> Either ParseError (Table5,Txt) -- variables left undeleted 


convert5' _ _((_ ,s:|_  ,_ ),[]                    ) = Right (s,pack "")


convert5' m f((n ,s:|st ,ls), Top(DEF ide     ):xs) 
 | isJust(M.lookup ide s)                           = makeErr(msgIde ide "is already defined")(f++"--step5'") 0 0
 | otherwise                                        = do
  new <- minUnused m ls f
  {-(("begin_static_assert "++show new++"; ")++)<$$>-}
  convert5' m f((n, M.insert ide new s :| st,new:ls),xs)


convert5' m f((n ,s:|st ,ls),Top(DEL ide     ):xs) = case M.lookup ide s of
   Just  k                                      -> {-(("unsafe_assert "++show k++"; ")++)<$$>-}convert5' m f((n, M.delete ide s :| st,filter (/=k) ls),xs)
   Nothing                                      -> makeErr(msgIde ide "is not defined or is already deleted in this scope")(f++"--step5'") 0 0
   
convert5' m f((n ,st    ,ls),Top(AS0 ide     ):xs) = case lookup' ide (toList st) of
   Just  _                                      -> {-(("unsafe_assert "++show k++"; ")++)<$$>-}convert5' m f((n, st,ls),xs)
   Nothing                                      -> makeErr(msgIde ide "is not defined or is already deleted")(f++"--step5'") 0 0

convert5' m f(state         ,Top(NUL sp      ):xs) = sp <+++$$> convert5' m f(state,xs) 


convert5' m f((n ,st    ,ls),Top(ADD ide   nm):xs) = case lookup' ide (toList st) of
   Just  k                                      -> ("mov "++show k{-++"; end_static_assert "++show k-}++"; inc "++nm++"; ") <+++$$> convert5' m  f((n,st,ls),xs)
   Nothing                                      -> makeErr(msgIde ide "is not defined")(f++"--step5'") 0 0
   
convert5' m f((n ,st    ,ls),Top(SUB ide   nm):xs) = case lookup' ide (toList st) of
   Just  k                                      -> ("mov "++show k{-++"; end_static_assert "++show k-}++"; dec "++nm++"; ") <+++$$> convert5' m  f((n,st,ls),xs)
   Nothing                                      -> makeErr(msgIde ide "is not defined")(f++"--step5'") 0 0

convert5' m f((n ,st    ,ls),Top(REA ide     ):xs) = case lookup' ide (toList st) of
   Just  k                                      -> ("mov "++show k{-++"; end_static_assert "++show k-}++"; _input; ") <+++$$> convert5'  m f((n,st,ls),xs)
   Nothing                                      -> makeErr(msgIde ide "is not defined")(f++"--step5'") 0 0

convert5' m f((n ,st    ,ls),Top(WRI ide     ):xs) = case lookup' ide (toList st) of
   Just  k                                      -> ("mov "++show k++"; output; ") <+++$$> convert5'  m f((n,st,ls),xs)
   Nothing                                      -> makeErr(msgIde ide "is not defined")(f++"--step5'") 0 0

convert5' m f(state         ,Null             :xs) = " " <+++$$> convert5' m f(state,xs)
convert5' m f(state         ,Top(COM cm)      :xs) = cm  <+++$$> convert5' m f(state,xs)

convert5' m f((n ,st    ,ls),Bot(WHI ide,Ns v):xs) = case lookup' ide (toList st) of
   Just k                                       -> do
    (table1,res1) <- convert5'  m f((n+1,M.empty `cons` st,ls),v ) -- inside the loop
    if not(M.null table1) 
     then let leftList = map fst $ _MtoList table1 in 
     makeErr(Message$identMsg leftList)(f ++ "--step5'") 0 0
     else do
      (table2,res2) <- convert5'  m f((n  ,st               ,ls),xs) -- left
      return (table2,"mov " <+> show k <+> "; loop; " <+> res1 <+> "mov " <+> show k <+> "; pool; " <+> res2)
      {-++ "begin_static_assert "++show k++"; "-}
   Nothing                                      -> makeErr(msgIde ide "is not defined")(f ++ "--step5'") 0 0
   
convert5' m f((n ,st    ,ls),Bot(BLO ,Ns v):xs) =  do
    (table1,res1) <- convert5'  m f((n+1,M.empty `cons` st,ls),v ) -- inside the loop
    if not(M.null table1) 
     then let leftList = map fst $ _MtoList table1 in 
     makeErr(Message$identMsg leftList)(f++"--step5'") 0 0
     else do
      (table2,res2) <- convert5'  m f((n  ,st               ,ls),xs) -- left
      return (table2,res1 <+> res2)

identMsg :: [String] -> String
identMsg qs = case qs of 
 []  -> "" 
 [q] -> "identifier "++q++" is not deleted"
 rs  -> "identifiers "++show rs++" are not deleted"