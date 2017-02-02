{-# LANGUAGE ForeignFunctionInterface #-}
module HiddenChar.HiddenChar
(getHiddenChar
)where
import Data.Char
-- import Foreign.C.Types
import System.IO 
{-
getHiddenChar :: IO Char
getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
  
  

-- copied from http://stackoverflow.com/questions/2983974/haskell-read-input-character-from-console-immediately-not-after-newline  

-}

getHiddenChar :: IO Char
getHiddenChar  = do 
 hSetEcho stdin False
 c <- getChar
 hSetEcho stdin True
 return c
 
-- copied from http://www.cs.nott.ac.uk/~gmh/errata.html