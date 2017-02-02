{-# OPTIONS -Wall #-}
module Camphor.SafePrelude(
Bool(..),(&&),(||),not,otherwise
,Maybe(..),maybe
,Either(..),either
,Ordering(..)
,Char,String
,fst,snd,curry,uncurry
,Eq,(==),(/=)
,Ord,compare,(<),(>=),(>),(<=),max,min
,Int,Integer
,Num,(+),(*),(-)
,Integral,toInteger
,Monad,(>>=),return,(>>),fail
,Functor,fmap
,mapM,mapM_,sequence,sequence_,(=<<)
,id,const,(.),flip,($),until,asTypeOf
,seq,($!)
,map,(++),filter,null,length
,reverse,foldl,foldr
,and,or,any,all,concat,concatMap
,scanl,scanl1,scanr,scanr1
,iterate,repeat,replicate,cycle
,take,drop,splitAt,takeWhile,dropWhile,span,break
,elem,notElem,lookup
,zip,zip3,zipWith,zipWith3,unzip,unzip3
,lines,words,unlines,unwords
,ShowS,Show,showsPrec,show,showList,shows,showChar,showString,showParen
-- ,ReadS,Read,readsPrec,readList,reads,readParen,lex
,IO,putChar,putStr,putStrLn,print,getChar,getLine,getContents,interact
,FilePath,readFile,writeFile,appendFile,readIO,readLn
)where