{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.TupleTrans
(getFst,getSnd
,askFst,askSnd
,putFst,putSnd
,modifyFst,modifySnd
)where
import Control.Monad.State
import Control.Monad.Reader

getFst :: (Monad m) => StateT (a,b) m a
getFst = do{(a,_) <- get; return a}

getSnd :: (Monad m) => StateT (a,b) m b
getSnd = do{(_,b) <- get; return b}

askFst :: (Monad m) => ReaderT (a,b) m a 
askFst = do{(a,_) <- ask; return a}

askSnd :: (Monad m) => ReaderT (a,b) m b
askSnd = do{(_,b) <- ask; return b}
 
putFst :: (Monad m) => a -> StateT (a,b) m () 
putFst a = do{(_,b) <- get; put (a,b)}

putSnd :: (Monad m) => b -> StateT (a,b) m ()  
putSnd b = do{(a,_) <- get; put (a,b)}

modifyFst :: (Monad m) => (a -> a) -> StateT (a,b) m ()   
modifyFst f = do{(a,b) <- get; put (f a,b)}

modifySnd :: (Monad m) => (b -> b) -> StateT (a,b) m ()  
modifySnd g = do{(a,b) <- get; put (a,g b)}