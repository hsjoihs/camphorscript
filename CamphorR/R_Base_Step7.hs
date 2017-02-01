{-# OPTIONS -Wall #-}
module CamphorR.R_Base_Step7
(step7_R
,parser7_R
,convert7_R
)where

import Text.Parsec hiding(token)
import Text.Parsec.Error
import Text.Parsec.Pos
import CamphorR.R_Base_Step8(parser8_R)

parser7_R :: String -> Either ParseError String
parser7_R xs = parser7_R' ([],0,(1,1)) xs

type State = ([Int],Int,(Int,Int)) --loop addresses(inner=head),current address,position


parser7_R' :: State -> String -> Either ParseError String
 