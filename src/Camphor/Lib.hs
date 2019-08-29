{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall #-}
module Camphor.Lib
(lib_dir
,default_lib_extension
) where
import Camphor.SafePrelude
lib_dir :: FilePath
lib_dir = "lib"

default_lib_extension :: FilePath
default_lib_extension = "txt"