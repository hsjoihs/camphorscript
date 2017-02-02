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
,mapM,mapM_,forM,forM_,sequence,sequence_,(=<<)
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
,genericLength,genericReplicate,genericTake,genericDrop,genericSplitAt
,catMaybes,fromMaybe,isJust,isNothing,listToMaybe,mapMaybe,maybeToList
,isSpace,isAlpha,isAlphaNum,toLower,toUpper
,Identity(..),when
,mappend,mempty,mconcat,Monoid
,comparing
,ap,liftM,Applicative,(>=>),(<=<),unless
,first,second
,(<++>),(<:>),(<$$>),(</>)
,ZipList(..)
,ord,chr
,filterM
,isPrefixOf,isInfixOf,isSuffixOf
,maybeToEither,liftE
,readMay
,Foldable(),fold,foldMap,foldr',foldl',foldrM,foldlM,traverse_,for_,sequenceA_,asum,msum,toList,sum,product,maximum,maximumBy,minimum,minimumBy,find
,_MtoList
)where
import Prelude hiding(fst,snd,foldr,foldl,mapM_,sequence_,concat,concatMap,and,or,any,all,sum,product,maximum,minimum,elem,notElem)
import Camphor.Tuple
import Control.Monad(join,guard,when,ap,liftM,(>=>),(<=<),unless,forM,filterM)
import Control.Applicative((<*>),(<$>),pure,Applicative,ZipList(..))
import Data.List(genericLength,genericReplicate,genericTake,genericDrop,genericSplitAt,isPrefixOf,isInfixOf,isSuffixOf)
import Data.Maybe(catMaybes,fromMaybe,isJust,isNothing,listToMaybe,mapMaybe,maybeToList)
import Data.Char(isSpace,isAlpha,isAlphaNum,toLower,toUpper,ord,chr)
import Data.Functor.Identity(Identity(..))
import Data.Monoid(mappend,mempty,mconcat,Monoid)
import Data.Ord(comparing)
import Control.Arrow(first,second)
import System.FilePath((</>))
import Data.Foldable(Foldable(),fold,foldMap,foldr,foldr',foldl,foldl',foldrM,foldlM,traverse_,for_,sequenceA_,asum,mapM_,forM_,sequence_,msum,toList,concat,concatMap,and,or,any,all,sum,product,maximum,maximumBy,minimum,minimumBy,elem,notElem,find)
import qualified Data.Map as M

_MtoList :: M.Map a b -> [(a,b)]
_MtoList = M.toList

infixr 5 <++>
(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

infixl 4 <$$>
(<$$>) :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)
(<$$>) = fmap . fmap 


data Between a b = East a | West b deriving(Show, Eq, Ord)
showStr :: String -> String
showStr = show 

showNum :: (Num a,Show a) => a -> String
showNum = show 

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _   (Just x) = Right x
maybeToEither err Nothing  = Left err

liftE :: (e -> Maybe a) -> e -> Either e a
liftE f = maybeToEither <*> f

readMay :: Read a => String -> Maybe a
readMay s      = case [x | (x,t) <- reads s, ("","") <- lex t] of [x] -> Just x ; _ -> Nothing 