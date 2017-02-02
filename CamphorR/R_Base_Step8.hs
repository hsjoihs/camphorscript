{-# OPTIONS -Wall #-}
module CamphorR.R_Base_Step8
(step8_R
,parser8_R
,convert8_R
)where

import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,(!!),read)
step8_R :: String -> Either a String
step8_R  = Right . convert8_R . parser8_R

convert8_R :: [String] -> String
convert8_R xs=convert8_R' xs 0

convert8_R' :: [String] -> Int -> String --depth
convert8_R' []     _   = ""
convert8_R' (x:xs) n = case reverse x of 
 '[' : _ -> replicate n     '\t'++x++"\n"++convert8_R' xs (n+1)
 ']' : _ -> replicate (n-1) '\t'++x++"\n"++convert8_R' xs (n-1)
 _       -> replicate n     '\t'++x++"\n"++convert8_R' xs n




parser8_R :: String -> [String]
parser8_R xs = parser8_R' xs ""

parser8_R'::String->String->[String] --  input buffer(reverse) result
parser8_R'    ""         ""       = []
parser8_R'    ""         xs       = [reverse xs]
parser8_R'   (x:xs)      ""      
 | x `elem` "+-><.,[]"            = parser8_R' xs [x]
 | otherwise                      = parser8_R' xs ""
 
parser8_R'   (x:xs)     ys@(y:_)   
 | not(x `elem` "+-><.,[]")               = parser8_R' xs ys
 | x `canFollow` y                        = parser8_R' xs (x:ys)   
 | otherwise                              = reverse ys: parser8_R' xs [x]
 
canFollow::Char->Char->Bool
canFollow '+' =(`elem` "+<>")
canFollow '-' =(`elem` "-<>")
canFollow '>' =(=='>')
canFollow '<' =(=='<')
canFollow '.' =(`elem` "+-<>")
canFollow ',' =(`elem` "+-<>")
canFollow '[' =(`elem` "<>")
canFollow ']' =(`elem` "<>")
canFollow _   =const False