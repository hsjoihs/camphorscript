{-# OPTIONS -Wall #-}
{- translation -}
module Camphor.Base_Step4
(parser4
,Com4(..)
,sentences_
,Tree(..)
,convert4
,convert4'
,step4
) where


import Camphor.Global
import Text.Parsec hiding(token)
import Text.Parsec.Error
import Text.Parsec.Pos
import Control.Applicative hiding ((<|>),many)
import qualified Data.Map as M
import Camphor.Base_Step4_1


-- data Com4=DEF|DEL|ADD|SUB|WHI|REA|WRI|NUL|EMP|COM deriving(Show)
-- type Set4=(Com4,Ident,Tree [Char] Com4 [Char])

type VarNum=Integer
type Table4=M.Map Ident VarNum -- variable, variable num
type CurrState=(Int,[Table4],[VarNum]) -- block num, defined variables(inner scope first), used variable num
{- Table4 must not be empty -}

step4::String->Either ParseError String
step4 str=do{sets<-parse parser4 "step4" str;convert4 sets}

convert4::[Set4]->Either ParseError String
convert4 xs=convert4' ((1,[M.empty],[]),xs)

minUnused::[VarNum]->VarNum
minUnused xs     = head$filter(\x->not(x `elem` xs)) [0..]

remove::VarNum->[VarNum]->[VarNum]
remove x xs = filter (/=x) xs

lookup'::Ord k=>k->[M.Map k a]->Maybe a
lookup' _ []     = Nothing
lookup' i (t:ts) = case(M.lookup i t)of Just a->Just a; Nothing-> lookup' i ts

convert4'::(CurrState,[Set4])->Either ParseError String


convert4'( _            ,[]                    ) = Right ""

convert4'((_ ,[]    ,_ ),_                     ) = error "Invalid call of convert4'"

convert4'((n ,(s:st),ls),((DEF,ide,_      ):xs)) 
 | isJust(M.lookup ide s)                        = Left $newErrorMessage (Message$"identifier "++show ide++"is already defined")(newPos "step4" 0 0)
 | otherwise                                     = convert4' ((n, M.insert ide new s : st,new:ls),xs)
  where new=minUnused ls

convert4'((n ,(s:st),ls),((DEL,ide,_      ):xs)) = case (M.lookup ide s) of
   Just  k                                      -> convert4' ((n, M.delete ide s : st,remove k ls),xs)
   Nothing                                      -> Left $newErrorMessage (Message$"identifier "++show ide++"is not defined in this scope")(newPos "step4" 0 0)

convert4'(state         ,((NUL,_  ,Node sp):xs)) = (sp++)<$>convert4'(state,xs) 
convert4'(_             ,((NUL,_  ,_      ):_ )) = error "Invalid format" 


convert4'((n ,st    ,ls),((ADD,ide,Node nm):xs)) = case (lookup' ide st) of
   Just  k                                      -> (\x->"mov "++show k++"; inc "++nm++"; "++x)<$>convert4' ((n,st,ls),xs)
   Nothing                                      -> Left $newErrorMessage (Message$"identifier "++show ide++"is not defined")(newPos "step4" 0 0)
convert4'(_             ,((ADD,_  ,_      ):_ )) = error "Invalid format" 
   
convert4'((n ,st    ,ls),((SUB,ide,Node nm):xs)) = case (lookup' ide st) of
   Just  k                                      -> (\x->"mov "++show k++"; dec "++nm++"; "++x)<$>convert4' ((n,st,ls),xs)
   Nothing                                      -> Left $newErrorMessage (Message$"identifier "++show ide++"is not defined")(newPos "step4" 0 0)
convert4'(_             ,((SUB,_  ,_      ):_ )) = error "Invalid format" 

convert4'((n ,st    ,ls),((REA,ide,_      ):xs)) = case (lookup' ide st) of
   Just  k                                      -> (\x->"mov "++show k++"; _input; "++x)<$>convert4' ((n,st,ls),xs)
   Nothing                                      -> Left $newErrorMessage (Message$"identifier "++show ide++"is not defined")(newPos "step4" 0 0)

convert4'((n ,st    ,ls),((WRI,ide,_      ):xs)) = case (lookup' ide st) of
   Just  k                                      -> (\x->"mov "++show k++"; output; "++x)<$>convert4' ((n,st,ls),xs)
   Nothing                                      -> Left $newErrorMessage (Message$"identifier "++show ide++"is not defined")(newPos "step4" 0 0)

convert4'(state         ,((EMP,_  ,_      ):xs)) = (' ':)<$>convert4'(state,xs)

convert4'(state         ,((COM,_  ,Node cm):xs)) = (cm++)<$>convert4'(state,xs)
convert4'(_             ,((COM,_  ,_      ):_ )) = error "Invalid format" 

convert4'((n ,st    ,ls),((WHI,ide,Nodes v):xs)) = case (lookup' ide st) of
   Just k                                       -> 
    (\x->"mov "++show k++"; loop; "++x)<$>convert4' ((n+1,M.empty:st,ls),v)<++>Right("mov "++show k++"; pool; ")<++>convert4'((n,st,ls),xs)
   Nothing                                      -> Left $newErrorMessage (Message$"identifier "++show ide++"is not defined")(newPos "step4" 0 0)
convert4'(_             ,((WHI,_  ,_      ):_ )) = error "Invalid format" 