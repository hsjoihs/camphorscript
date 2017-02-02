{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Base_Step2.Auxilary
(getOpContents2
,canBeRightOf
,canBeLeftOf
,toList1,toList2,toOpList,getOpFixity,getOpsFixities,makeReplacerTable,makeReplacerTable2,ReplTable,isConsistent,contradiction
) where
import Prelude hiding(head,tail,init,last,minimum,maximum,foldl1,foldr1,scanl1,scanr1,(!!),read,error,undefined)
import Camphor.Base_Step2.Base_Step2_2
import Camphor.Base_Step2.UserState
import Camphor.Global.Synonyms
import Camphor.Global.Utilities
import Text.Parsec 
import qualified Data.Map as M

type ReplTable = M.Map Ident Value 
getOpContents2 :: SourcePos -> UserState -> Oper -> Either ParseError OpInfo
getOpContents2 pos s o = case getOpContents s o of
 Nothing   -> Left $ newErrorMessage(Message$"operator "++show o++" is not defined")pos
 Just info -> Right$ info

 
canBeRightOf :: SourcePos -> Fixity -> Fixity -> Either ParseError ()
canBeRightOf pos f2 f1
 | v2 > v1 = Right()
 | v2 < v1 = __mkmsg pos (getOpName f1) (getOpName f2)
 where v1 = getFixValue f1; v2 = getFixValue f2;
canBeRightOf pos (InfixL _ nm1) (InfixL _ nm2) = __mkmsg pos nm1 nm2
canBeRightOf pos (InfixL _ nm1) (InfixR _ nm2) = __mixed pos nm1 nm2
canBeRightOf pos (InfixR _ nm1) (InfixL _ nm2) = __mixed pos nm1 nm2
canBeRightOf _   (InfixR _ _  ) (InfixR _ _  ) = Right()

canBeLeftOf :: SourcePos -> Fixity -> Fixity -> Either ParseError ()
canBeLeftOf pos f2 f1
 | v2 > v1 = Right()
 | v2 < v1 = __mkmsg pos (getOpName f1) (getOpName f2)
 where v1 = getFixValue f1; v2 = getFixValue f2;
canBeLeftOf pos (InfixR _ nm1) (InfixR _ nm2) = __mkmsg pos nm1 nm2
canBeLeftOf pos (InfixR _ nm1) (InfixL _ nm2) = __mixed pos nm1 nm2
canBeLeftOf pos (InfixL _ nm1) (InfixR _ nm2) = __mixed pos nm1 nm2
canBeLeftOf _   (InfixL _ _  ) (InfixL _ _  ) = Right()

__mkmsg :: SourcePos -> Oper -> Oper -> Either ParseError a
__mkmsg pos nm1 nm2 = Left $ newErrorMessage(Message$"operator "++show nm2++" has smaller fixity than its outer operator "++nm1)pos
 
__mixed :: SourcePos -> Oper -> Oper -> Either ParseError a
__mixed pos nm1 nm2 = Left $ newErrorMessage(Message$"operator "++show nm1++" and operator "++show nm2++" has opposite fixity and thus cannot coexist")pos  
 
 
toList1 :: TypeList -> [Ident]
toList1 (_,t,xs) = t:[x|(_,_,x)<-xs]
 
toList2 :: ValueList -> [Value]
toList2 (v,xs) = v:map snd xs 

toOpList :: ValueList -> [Oper]
toOpList (_,xs) = map fst xs

getOpFixity :: SourcePos -> UserState -> Oper -> Either ParseError Fixity
getOpFixity pos stat op = fmap fst $ getOpContents2 pos stat op  
 
getOpsFixities :: SourcePos -> UserState -> ValueList -> Either ParseError [Fixity]
getOpsFixities pos stat valuelist = mapM (getOpFixity pos stat) $ toOpList valuelist

makeReplacerTable :: TypeList -> ValueList -> ReplTable
makeReplacerTable tlist vlist = M.fromList$zip(toList1 tlist)(toList2 vlist) 

makeReplacerTable2 :: (TypeList,TypeList) -> (ValueList,ValueList) -> ReplTable
makeReplacerTable2 (t1,t2)(v1,v2) = M.fromList$zip(toList1 t1++toList1 t2)(toList2 v1++toList2 v2)

isConsistent :: [Fixity] -> Bool
isConsistent xs = all isInfixL xs || all isInfixR xs

contradiction :: [Fixity] -> Maybe Fixity
contradiction [ ] = Nothing
contradiction [_] = Nothing
contradiction (InfixL _ _:xs) = case filter isInfixR xs of [] -> Nothing; (x:_) -> Just x
contradiction (InfixR _ _:xs) = case filter isInfixL xs of [] -> Nothing; (x:_) -> Just x
