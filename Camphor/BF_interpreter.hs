{-# LANGUAGE FlexibleContexts ,NoImplicitPrelude, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Camphor.BF_interpreter
(interpreterIO
,pureInterpreter
)where
import Camphor.SafePrelude 
import Camphor.Global.Synonyms
import Camphor.IO
import Camphor.InfList
import Camphor.BF_parser
import Text.Parsec
import Camphor.Transformer
import Data.Word (Word8)
import HiddenChar.HiddenChar
import Camphor.VirtualIO

type VIO = VirtualIO Char Char 

interpreterIO :: Txt -> IO ()
interpreterIO str = case parse parseBF' "" str of
 Left e -> outputErr e
 Right xs -> runBF xs
 
pureInterpreter :: Txt -> Either ParseError (String -> Maybe String)
pureInterpreter str = unwrapVIO <$> runBF <$> parse parseBF' "" str 
 
data Stat = Stat {
  notyet :: [BFComm],
  mem :: (Integer,InfList Word8)
} 


class (Functor io, Monad io) => IOLike io where
 getChar' :: io (Maybe Char)
 putChar' :: Char -> io ()
 liftIO' :: io a -> StateT s io a
 
instance IOLike IO where
 getChar' = Just <$> getHiddenChar
 putChar' = putChar
 liftIO' = liftIO 
 
instance IOLike VIO where
 getChar' = getI
 putChar' = putO
 liftIO' = lift
 
runBF :: (IOLike io) => [BFComm] -> io () 
runBF xs = evalStateT vm Stat{notyet = xs, mem = (0,initial)}

vm :: (IOLike io) => StateT Stat io ()
vm = get >>= \stat -> case notyet stat of
 []        -> return ()
 (comm:cs) -> do
  let (pos,cont) = mem stat
  case comm of
   INCR     -> modify(\s -> s{notyet = cs, mem = upd (+1)    $ mem stat}) 
   DECR     -> modify(\s -> s{notyet = cs, mem = upd (+(-1)) $ mem stat}) 
   LEFT     -> modify(\s -> s{notyet = cs, mem = (pos-1,cont)}) 
   RIGHT    -> modify(\s -> s{notyet = cs, mem = (pos+1,cont)}) 
   INPUT    -> do{
   Just c <- (fromIntegral . ord) <$$> liftIO' getChar'; -- fail is called
   modify(\s -> s{ notyet = cs, mem = upd (const c) $ mem stat}); 
   } 
   OUTPUT   -> modify(\s -> s{notyet = cs}) >> liftIO' (putChar' . chr . fromIntegral $ cont `at` pos) >> return ()
   WHILE xs 
    | cont `at` pos == 0 -> modify(\s -> s{notyet = cs}) 
    | otherwise          -> do{Stat{mem = newM} <- liftIO' $ execStateT vm Stat{notyet = xs, mem = mem stat}; modify(\s -> s{mem = newM})}
  vm
  
  

-- modified from https://github.com/eagletmt/bf-interp