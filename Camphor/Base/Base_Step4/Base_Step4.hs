{-# LANGUAGE FlexibleContexts ,NoImplicitPrelude #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{- Desugaring -}
module Camphor.Base.Base_Step4.Base_Step4
(step4
,To4
-- ,parser4
-- ,convert4
) where

import Camphor.SafePrelude 
import Camphor.Show
import Camphor.NonEmpty
import Camphor.Global.Utilities
import Camphor.Global.Synonyms
import Camphor.Base.Base_Step4.Simplify
import Camphor.Base.Base_Step4.IdentFactory
import Camphor.Base.Base_Step4.Parsers
import Text.Parsec hiding(token)
import qualified Data.Map as M

type To4 = Bool



step4 :: To4 -> FilePath -> Txt -> Either ParseError Txt
step4 to4 file str = do
 let incdecmerge = to4
 sets <- parse parser4' (file ++ "--step4_II") str
 let sets2 = (if incdecmerge then simple4 else id) sets
 convert4 file sets2
 

-- addAssert :: [Set4] -> [Set4]

type Table4 = M.Map Ident2 () -- variable, variable address
type CurrState = (Int,NonEmpty Table4) -- block num, defined variables(inner scope first)
{- Table4 must not be empty -}

convert4 ::  FilePath -> [Set4] -> Either ParseError Txt
convert4 file xs = snd <$> convert4' Nothing file ((1,M.empty :| []),xs)

msgIde :: Ident2 -> String -> Message
msgIde ide left= Message$"identifier "++unId ide++" "++left


lookup' :: Ord k => k -> [M.Map k a] -> Bool -- lookup towards the outer scope until you find a variable
lookup' _ []     = False
lookup' i (t:ts) = case M.lookup i t of
  Just _  -> True
  Nothing -> lookup' i ts

convert4' :: Maybe MemSize -> FilePath -> (CurrState,[Set4]) -> Either ParseError (Table4,String) -- variables left undeleted 


convert4' _ _((_ ,s:|_  ),[]           ) = Right (s,"")


convert4' m f((n ,s:|st ),DEF ide   :xs) 
 | M.member ide s                           = makeErr(msgIde ide "is already defined")(f++"--step4'") 0 0
 | otherwise                                        = do
  (("char "++unId ide++"; ")++)<$$> convert4' m f((n, M.insert ide () s :| st),xs)


convert4' m f((n ,s:|st ),DEL ide   :xs) = case M.member ide s of
 True  -> ("delete "++unId ide++"; ") <++$$> convert4' m f((n, M.delete ide s :| st),xs)
 False -> makeErr(msgIde ide "is not defined or is already deleted in this scope")(f++"--step4'") 0 0
   
convert4' m f((n ,st    ),AS0 ide   :xs) = case lookup' ide (toList st) of
 True  -> ("assert_zero "++unId ide++"; ") <++$$> convert4' m f((n, st),xs)
 False -> makeErr(msgIde ide "is not defined or is already deleted")(f++"--step4'") 0 0

convert4' m f(state      ,NUL sp    :xs) = (sp++) <$$> convert4' m f(state,xs) 


convert4' m f((n ,st    ),ADD ide nm:xs) = case lookup' ide (toList st) of
 True  
  | nm > 0    -> (unId ide++"+="++showNum   nm ++";") <++$$> convert4' m  f((n,st),xs) 
  | nm < 0    -> (unId ide++"-="++showNum (-nm)++";") <++$$> convert4' m  f((n,st),xs) 
  | otherwise ->                                             convert4' m  f((n,st),xs) 
 False -> makeErr(msgIde ide "is not defined")(f++"--step4'") 0 0
   


convert4' m f((n ,st    ),REA ide   :xs) = case lookup' ide (toList st) of
 True  -> ("read( "++unId ide++");") <++$$> convert4'  m f((n,st),xs)
 False -> makeErr(msgIde ide "is not defined")(f++"--step4'") 0 0

convert4' m f((n ,st    ),WRI ide   :xs) = case lookup' ide (toList st) of
 True  -> ("write("++unId ide++");") <++$$> convert4' m f((n,st),xs)
 False -> makeErr(msgIde ide "is not defined")(f++"--step4'") 0 0

convert4' m f(state      ,Null      :xs) = " " <++$$> convert4' m f(state,xs)
convert4' m f(state      ,COM cm    :xs) = cm  <++$$> convert4' m f(state,xs)

convert4' m f((n ,st    ),WHI ide(Ns v):xs) = case lookup' ide (toList st) of
 True  -> do
  (table1,res1) <- convert4'  m f((n+1,M.empty `cons` st),v ) -- inside the loop
  if not(M.null table1) 
   then let leftList = map fst $ _MtoList table1 in 
   makeErr(Message$identMsg leftList)(f ++ "--step4'") 0 0 
   else do
  (table2,res2) <- convert4'  m f((n  ,st               ),xs) -- left
  return (table2,"while(" ++ unId  ide ++ "){ " ++ res1 ++ "} " ++ res2)
 False -> makeErr(msgIde ide "is not defined")(f ++ "--step4'") 0 0
   
convert4' m f((n ,st    ),BLO (Ns v):xs) =  do
 (table1,res1) <- convert4'  m f((n+1,M.empty `cons` st),v ) -- inside the loop
 if not(M.null table1) 
  then let leftList = map fst $ _MtoList table1 in 
  makeErr(Message$identMsg leftList)(f++"--step4'") 0 0
  else do
 (table2,res2) <- convert4'  m f((n  ,st               ),xs) -- left
 return (table2,"{"++res1++"}" ++ res2)

identMsg :: [Ident2] -> String
identMsg qs = case map unId qs of 
 []  -> "" 
 [q] -> "identifier "++q++" is not deleted"
 rs  -> "identifiers "++show rs++" are not deleted"