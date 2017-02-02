{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall  #-}
module Camphor.Base.Base_Step2.New
(newC,newD,newF1,newF2,newFix,newS
) where 

import Camphor.Base.Base_Step2.Type
import Camphor.SafePrelude
import Camphor.Base.Base_Step2.UserState
import Camphor.Base.Base_Step2.Auxilary2
import Camphor.Base.Base_Step2.ErrList
import Camphor.Global.Synonyms
import Text.Parsec 

newC :: SourcePos -> Ident2 -> UserState -> Either ParseError UserState
newC pos ident stat 
 | stat `containsIdent` ident = Left $toPE pos$ Step2 <!> Access <!> WrongDef_3 <!> Alreadydefined <!> Idn ident
 | otherwise                  = Right$addVar stat ident
 
newD :: SourcePos -> Ident2 -> UserState -> Either ParseError UserState
newD pos ident stat 
 | stat `containsIdent` ident = Right$removeIdent stat ident -- functions and syntaxes can also be deleted
 | otherwise                  = Left $toPE pos$ Step2 <!> Access <!> WrongDel <!> Notdefined_2 <!> Idn ident

 
newFix :: SourcePos -> Fix -> Oper -> (Fix -> Oper -> Fixity) -> UserState -> Either ParseError UserState
newFix pos fixity op infi stat = case getOpContents stat op of
 Nothing     -> Right$addOpFixity stat (infi fixity op)
 Just(fix,_) 
  | fix == infi fixity op -> Right stat 
  | otherwise             -> Left$toPE pos $ Step2 <!> Fixity <!> WrongDef_2 <!> Conflictfixdef <!> Operat_3 op
 

-- Function definition
newF1 :: SourcePos -> Ident2 -> TypeList -> Maybe Sent -> UserState -> Either ParseError UserState
newF1 pos name typelist sent stat
 | typelistIdentConflict typelist = Left $toPE pos$ Step2 <!> Type <!> WrongDef <!> Paramoverlap  <!> Functi name
 | otherwise = case getVFContents stat name of
  Just Variable  -> Left $toPE pos$ Step2 <!> Type <!> WrongDef <!> Definedasvar_2 <!> Functi_2 name
  Nothing        -> Right$addFunSyn stat name [(typelist,sent)] []
  Just(FunSyn xs ys) 
   | any (\(tlist,_) -> typelist `overlaps` tlist) xs   -> Left $ toPE pos$ Step2 <!> Type <!> WrongDef <!> Typeoverlap <!> Functi name
   | otherwise                                          -> return $ addFunSyn stat name ((typelist,sent):xs) ys
 
-- Operator definition 
newF2 :: SourcePos -> Oper -> TypeList -> TypeList -> Maybe Sent -> UserState -> Either ParseError UserState
newF2 pos op typelist1 typelist2 sent stat = 
 addOpContents stat op (typelist1,typelist2,fmap toSent2 sent) $ 
  (toPE pos$ Step2 <!> Type <!> WrongDef <!> Fixnotdefined <!> Operat_3 op,
   toPE pos$ Step2 <!> Type <!> WrongDef <!> Typeoverlap   <!> Operat   op,
   toPE pos$ Step2 <!> Type <!> WrongDef <!> Paramoverlap  <!> Operat   op)

bothTLIC :: Between TailTypeList TypeList -> Bool
bothTLIC (West a) = typelistIdentConflict a
bothTLIC (East a) = typelistIdentConflict a
 
overlaps4 :: Between TailTypeList TypeList -> Between TailTypeList TypeList -> Bool
(West tl)  `overlaps4` (West tl2)  = tl `overlaps` tl2
(East ttl) `overlaps4` (East ttl2) = ttl `overlaps'` ttl2
_          `overlaps4` _           = False


 
newS :: SourcePos -> Ident2 -> Between TailTypeList TypeList -> Sent -> UserState -> Either ParseError UserState
newS pos name tl_ttl sent stat 
 | bothTLIC tl_ttl = Left$toPE pos$ Step2 <!> Type <!> WrongDef <!> Paramoverlap  <!> Synt name
 | otherwise = case getVFContents stat name of
  Just Variable  -> Left $toPE pos$ Step2 <!> Type <!> WrongDef <!> Definedasvar_2 <!> Synt_2 name
  Nothing        -> Right$addFunSyn stat name [] [(tl_ttl,sent)]
  Just(FunSyn xs ys) 
   | any (\(tlist,_) -> tl_ttl `overlaps4` tlist) ys -> Left $toPE pos$ Step2 <!> Type <!> WrongDef <!> Typeoverlap <!> Synt name
   | otherwise                                     -> return $ addFunSyn stat name xs ((tl_ttl,sent):ys)
  