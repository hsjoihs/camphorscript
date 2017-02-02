{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall  #-}
module Camphor.Base.Base_Step2.Def
(defC,defD,defF1,defF2,defFix,defS
) where 

import Camphor.SafePrelude
import Camphor.Global.Synonyms
import Camphor.Base.Base_Step2.Type
import Camphor.Base.Base_Step2.UserState
import Camphor.Base.Base_Step2.Auxilary
import Camphor.Base.Base_Step2.Auxilary2
import Camphor.Base.Base_Step2.ErrList
import Text.Parsec 
import Control.Monad.State hiding(fix)

defC :: SourcePos -> Ident2 -> StateT UserState (Either ParseError) ()
defC pos ident = do
 stat <- get
 if stat `containsIdent` ident 
  then err$toPE pos$ Step2 <!> Access <!> WrongDef_3 <!> Alreadydefined <!> Idn ident
  else put$addVar stat ident

defD :: SourcePos -> Ident2 -> StateT UserState (Either ParseError) ()
defD pos ident = do
 stat <- get
 case stat `removeIfContains` ident of
  Nothing -> err$toPE pos$ Step2 <!> Access <!> WrongDel <!> Notdefined_2 <!> Idn ident
  Just newStat -> put newStat
 
defFix :: SourcePos -> Fix -> Oper -> (Fix -> Oper -> Fixity) -> StateT UserState (Either ParseError) ()
defFix pos fixity op infi = do
 stat <- get
 case getOpContents stat op of
  Nothing     -> put $ addOpFixity stat (infi fixity op)
  Just(fix,_) 
   | fix == infi fixity op -> return ()
   | otherwise             -> err$toPE pos $ Step2 <!> Fixity <!> WrongDef_2 <!> Conflictfixdef <!> Operat_3 op
 

-- Function definition
defF1 :: SourcePos -> Ident2 -> TypeList -> Maybe Sent -> StateT UserState (Either ParseError) ()
defF1 pos name typelist sent 
 | typelistIdentConflict typelist = err$toPE pos$ Step2 <!> Type <!> WrongDef <!> Paramoverlap  <!> Functi name
 | otherwise = do
  stat <- get
  case getVFContents stat name of
   Just Variable  -> err$toPE pos$ Step2 <!> Type <!> WrongDef <!> Definedasvar_2 <!> Functi_2 name
   Nothing        -> put $ addFunSyn stat name [(typelist,sent)] []
   Just(FunSyn xs ys) 
    | any (\(tlist,_) -> typelist `overlaps` tlist) xs   -> err$ toPE pos$ Step2 <!> Type <!> WrongDef <!> Typeoverlap <!> Functi name
    | otherwise                                          -> put$addFunSyn stat name ((typelist,sent):xs) ys
 
 
-- Operator definition 
defF2 :: SourcePos -> Oper -> TypeList -> TypeList -> Maybe Sent -> StateT UserState (Either ParseError) ()
defF2 pos op typelist1 typelist2 sent = do
 stat <- get
 case addOpContents stat op (typelist1,typelist2,fmap toSent2 sent) of
  Left Notfound     -> err$ toPE pos$ Step2 <!> Type <!> WrongDef <!> Fixnotdefined <!> Operat_3 op
  Left Doubledefine -> err$ toPE pos$ Step2 <!> Type <!> WrongDef <!> Typeoverlap   <!> Operat   op
  Left Doubleparam  -> err$ toPE pos$ Step2 <!> Type <!> WrongDef <!> Paramoverlap  <!> Operat   op
  Right newStat     -> put newStat

 
bothTLIC :: Between TailTypeList TypeList -> Bool
bothTLIC (West a) = typelistIdentConflict a
bothTLIC (East a) = typelistIdentConflict a
 
overlaps4 :: Between TailTypeList TypeList -> Between TailTypeList TypeList -> Bool
(West tl)  `overlaps4` (West tl2)  = tl `overlaps` tl2
(East ttl) `overlaps4` (East ttl2) = ttl `overlaps'` ttl2
_          `overlaps4` _           = False


 
defS :: SourcePos -> Ident2 -> Between TailTypeList TypeList -> Sent -> StateT UserState (Either ParseError) ()
defS pos name tl_ttl sent  
 | bothTLIC tl_ttl = err$toPE pos$ Step2 <!> Type <!> WrongDef <!> Paramoverlap  <!> Synt name
 | otherwise = do
  stat <- get
  case getVFContents stat name of
   Just Variable  -> err$toPE pos$ Step2 <!> Type <!> WrongDef <!> Definedasvar_2 <!> Synt_2 name
   Nothing        -> put $ addFunSyn stat name [] [(tl_ttl,sent)]
   Just(FunSyn xs ys) 
    | any (\(tlist,_) -> tl_ttl `overlaps4` tlist) ys -> err$toPE pos$ Step2 <!> Type <!> WrongDef <!> Typeoverlap <!> Synt name
    | otherwise                                     -> put $ addFunSyn stat name xs ((tl_ttl,sent):ys)
  