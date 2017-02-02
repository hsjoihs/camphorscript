{-# LANGUAGE FlexibleContexts ,NoImplicitPrelude, GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.VirtualIO
(VirtualIO(),putO,getI,unwrapVIO
)where
import Camphor.SafePrelude 
import Camphor.Transformer

newtype VirtualIO i o a = VirtualIO {unVIO :: MaybeT (WriterT [o] (StateT [i] Identity)) a} deriving(Functor,Applicative,Monad) -- safe when failed

unwrapVIO :: VirtualIO i o () -> [i] -> Maybe [o]
unwrapVIO vio is = case (evalState . runWriterT . runMaybeT . unVIO) vio is of
 (Just (),os) -> Just os
 (Nothing,_ ) -> Nothing

putO :: o -> VirtualIO i o () 
putO o = VirtualIO (lift $ tell [o])

getI :: VirtualIO i o (Maybe i)
getI = VirtualIO $ do
 is <- lift $ lift $ get
 case is of
  [] -> return Nothing
  (x:xs) -> do{lift $ lift $ put xs; return (Just x)}