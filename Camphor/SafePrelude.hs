{-# OPTIONS -Wall #-}
module Camphor.SafePrelude(
Bool(..),(&&),(||),not,otherwise
,Maybe(..),maybe
,Either(..),either
,Ordering(..)
,Char,String
,fst,snd,trd
,pt1,pt2,pt3
,md1,md2,md3
,Fst,Snd,Trd
,curry,uncurry
,Eq,(==),(/=)
,Ord,compare,(<),(>=),(>),(<=),max,min
,Int,Integer
,Num,(+),(*),(-)
,Integral,toInteger,fromIntegral
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
,ShowS,Show,showsPrec,{-show,-}showList,shows,showChar,showString,showParen
,ReadS,Read,readsPrec,readList,reads,readParen,lex
,IO,putChar,putStr,putStrLn,print,getChar,getLine,getContents,interact
,FilePath,readFile,writeFile,appendFile,readIO,readLn
,Between(..)
,join,pure,(<*>),(<$>),guard
,showStr,showNum
,genericLength,genericReplicate,genericTake,genericDrop
,catMaybes,fromMaybe,isJust,isNothing,listToMaybe,mapMaybe,maybeToList
,isSpace,isAlpha,isAlphaNum,toLower,toUpper
,Identity(..),when
,mappend,mempty,mconcat,Monoid
,comparing
,ap,liftM,Applicative,(>=>),(<=<),unless
)where
import Prelude hiding(fst,snd)
import Camphor.Tuple
import Control.Monad(join,guard,when,ap,liftM,(>=>),(<=<),unless)
import Control.Applicative((<*>),(<$>),pure,Applicative)
import Data.List(genericLength,genericReplicate,genericTake,genericDrop)
import Data.Maybe(catMaybes,fromMaybe,isJust,isNothing,listToMaybe,mapMaybe,maybeToList)
import Data.Char(isSpace,isAlpha,isAlphaNum,toLower,toUpper)
import Data.Functor.Identity(Identity(..))
import Data.Monoid(mappend,mempty,mconcat,Monoid)
import Data.Ord(comparing)
data Between a b = East a | West b deriving(Show, Eq, Ord)
showStr :: String -> String
showStr s = show s

showNum :: (Num a,Show a) => a -> String
showNum s = show s