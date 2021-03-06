{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}
module Camphor.Transformer
(lift,liftIO,MonadTrans

,state,runState,evalState,execState,mapState,withState
,StateT(..),evalStateT,execStateT,mapStateT,withStateT
,get,put,modify

,Reader,reader,runReader,mapReader,withReader
,ReaderT(..),mapReaderT,withReaderT
,ask,local

,Writer,writer,runWriter,execWriter,mapWriter
,WriterT(..),execWriterT,mapWriterT
,tell,listen,pass,censor

,MaybeT(..),mapMaybeT

,IdentityT(..),mapIdentityT

,Except,except,runExcept,mapExcept,withExcept
,ExceptT(ExceptT),runExceptT,mapExceptT,withExceptT
,throwE,catchE

,getFst,getSnd,getTrd
,askFst,askSnd,askTrd
,putFst,putSnd,putTrd
,modifyFst,modifySnd,modifyTrd
)
where
import Camphor.SafePrelude
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except

-- tuples
getFst :: (Monad m, Fst t a) => StateT t m a
getFst = do{k <- get; return (fst k)}

getSnd :: (Monad m, Snd t b) => StateT t m b
getSnd = do{k <- get; return (snd k)}

getTrd :: (Monad m, Trd t c) => StateT t m c
getTrd = do{k <- get; return (trd k)}

askFst :: (Monad m, Fst t a) => ReaderT t m a 
askFst = do{k <- ask; return (fst k)}

askSnd :: (Monad m, Snd t b) => ReaderT t m b
askSnd = do{k <- ask; return (snd k)}

askTrd :: (Monad m, Trd t c) => ReaderT t m c
askTrd = do{k <- ask; return (trd k)}
 
putFst :: (Monad m, Fst t a) => a -> StateT t m () 
putFst a = do{k <- get; put (pt1 k a)}

putSnd :: (Monad m, Snd t b) => b -> StateT t m ()  
putSnd b = do{k <- get; put (pt2 k b)}

putTrd :: (Monad m, Trd t c) => c -> StateT t m ()  
putTrd c = do{k <- get; put (pt3 k c)}

modifyFst :: (Monad m, Fst t a) => (a -> a) -> StateT t m ()   
modifyFst f = do{k <- get; put (md1 f k)}

modifySnd :: (Monad m, Snd t b) => (b -> b) -> StateT t m ()  
modifySnd g = do{k <- get; put (md2 g k)}

modifyTrd :: (Monad m, Trd t c) => (c -> c) -> StateT t m ()  
modifyTrd h = do{k <- get; put (md3 h k)}
