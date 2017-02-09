{-# OPTIONS -Wall #-}
module CamphorR.R_Base_Step7
(step7_R
,parser7_R
-- ,convert7_R
,Type(..)
,tokenizeBf
)where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Text.Parsec hiding(token)
import Control.Applicative ()--hiding ((<|>),many)
import Camphor.Global.Utilities


-- desymbolization(already indented)
step7_R :: FilePath -> String -> Either ParseError String
step7_R = parser7_R 

parser7_R :: FilePath -> String -> Either ParseError String
parser7_R f xs = parser7_R' f([],0,(1,1)) (map tokenizeBf $ lines xs)

parser7_R' :: FilePath -> CurrState -> [[Tkn]] -> Either ParseError String
parser7_R' f state            ([]           :yss) = ('\n':)                           <$> parser7_R' f state yss
parser7_R' f state            (((sp,SPA):xs):yss) = (reverse sp++)                    <$> parser7_R' f state (xs:yss)
parser7_R' f state            (((pl,PLS):xs):yss) = (("inc "++show(length pl)++";")++)<$> parser7_R' f state (xs:yss)
parser7_R' f state            (((mn,MIN):xs):yss) = (("dec "++show(length mn)++";")++)<$> parser7_R' f state (xs:yss)
parser7_R' f state            (((_ , IN):xs):yss) = ( "_input;" ++)<$> parser7_R' f state (xs:yss)
parser7_R' f state            (((_ ,OUT):xs):yss) = ( "output;" ++)<$> parser7_R' f state (xs:yss)
parser7_R' f (ads   ,adr,pos) (((mv,MOV):xs):yss)
 | diff==0                                        = parser7_R' f (ads,adr,pos) (xs:yss) --no move
 | new  <0                                        = makeErr(Message "negative address")(f++"Rstep7'") 0 0
 | otherwise                                      = (("mov "++show new++";" )++)<$>parser7_R' f (ads,new,pos) (xs:yss)
 where diff = length(filter (=='>') mv) - length(filter (=='<') mv) ; new = adr+diff
parser7_R' f (ads   ,adr,pos) (((_ ,LOP):xs):yss) = (("mov "++show adr++";loop;")++)<$>parser7_R' f (adr:ads,adr,pos) (xs:yss) 
parser7_R' f ([]    ,_  ,_  ) (((_ ,POL):_ ):_  ) = makeErr(UnExpect "]")(f++"Rstep7'") 0 0
parser7_R' f (ad:ads,adr,pos) (((_ ,POL):xs):yss) 
 | ad==adr                                        = (("mov "++show adr++";pool;")++)<$>parser7_R' f (ads,adr,pos) (xs:yss) 
 | otherwise                                      = makeErr(Message "address is different")(f++"Rstep7'") 0 0
parser7_R' _ ([]    ,_  ,_  ) []                  = Right ""
parser7_R' f (_     ,_  ,_  ) []                  = makeErr(Expect "]")(f++"Rstep7'") 0 0


type CurrState = ([Int],Int,(Int,Int)) --loop addresses(inner=head),current address,position
type Tkn = (String,Type)
data Type = PLS | MIN | MOV | LOP | POL | IN | OUT | SPA  deriving (Show)

tokenizeBf :: String -> [Tkn]
tokenizeBf "" = []
tokenizeBf (v:ks) = merge v (tokenizeBf ks)
 where 
  merge :: Char -> [Tkn] -> [Tkn]
  merge '+' ((k,PLS):xs) = ('+':k,PLS):xs
  merge '+' ys           = ("+"  ,PLS):ys
  merge '-' ((k,MIN):xs) = ('-':k,MIN):xs
  merge '-' ys           = ("-"  ,MIN):ys
  merge '>' ((k,MOV):xs) = ('>':k,MOV):xs
  merge '>' ys           = (">"  ,MOV):ys
  merge '<' ((k,MOV):xs) = ('<':k,MOV):xs
  merge '<' ys           = ("<"  ,MOV):ys
  merge '[' ys           = (""   ,LOP):ys
  merge ']' ys           = (""   ,POL):ys
  merge ',' ys           = (""   ,IN ):ys
  merge '.' ys           = (""   ,OUT):ys
  merge x   ((k,SPA):xs) = (x  :k,SPA):xs
  merge x   ys           = ([x]  ,SPA):ys