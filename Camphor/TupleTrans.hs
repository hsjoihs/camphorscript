{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.TupleTrans
(getFst,getSnd,getTrd
,askFst,askSnd,askTrd
,putFst,putSnd,putTrd
,modifyFst,modifySnd,modifyTrd
)where
import Control.Monad.State
import Control.Monad.Reader

getFst :: (Monad m) => StateT (a,b,c) m a
getFst = do{(a,_,_) <- get; return a}

getSnd :: (Monad m) => StateT (a,b,c) m b
getSnd = do{(_,b,_) <- get; return b}

getTrd :: (Monad m) => StateT (a,b,c) m c
getTrd = do{(_,_,c) <- get; return c}

askFst :: (Monad m) => ReaderT (a,b,c) m a 
askFst = do{(a,_,_) <- ask; return a}

askSnd :: (Monad m) => ReaderT (a,b,c) m b
askSnd = do{(_,b,_) <- ask; return b}

askTrd :: (Monad m) => ReaderT (a,b,c) m c
askTrd = do{(_,_,c) <- ask; return c}
 
putFst :: (Monad m) => a -> StateT (a,b,c) m () 
putFst a = do{(_,b,c) <- get; put (a,b,c)}

putSnd :: (Monad m) => b -> StateT (a,b,c) m ()  
putSnd b = do{(a,_,c) <- get; put (a,b,c)}

putTrd :: (Monad m) => c -> StateT (a,b,c) m ()  
putTrd c = do{(a,b,_) <- get; put (a,b,c)}

modifyFst :: (Monad m) => (a -> a) -> StateT (a,b,c) m ()   
modifyFst f = do{(a,b,c) <- get; put (f a,b,c)}

modifySnd :: (Monad m) => (b -> b) -> StateT (a,b,c) m ()  
modifySnd g = do{(a,b,c) <- get; put (a,g b,c)}

modifyTrd :: (Monad m) => (c -> c) -> StateT (a,b,c) m ()  
modifyTrd h = do{(a,b,c) <- get; put (a,b,h c)}