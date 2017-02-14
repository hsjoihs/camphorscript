{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module HiddenChar.HiddenChar
(getHiddenChar
)where
import Data.Char
import Foreign.C.Types
import System.IO 


getHiddenChar :: IO Char

#if defined(__GLASGOW_HASKELL__) && ( defined(VERSION_Win32) || defined(VERSION_Win64) )

getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
  
#else

getHiddenChar = getChar

#endif  
  

-- copied from http://stackoverflow.com/questions/2983974/haskell-read-input-character-from-console-immediately-not-after-newline  
