{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall -fno-warn-unused-do-bind  #-}
module Camphor.Version
(version_num
)where

#include "version.h"

version_num :: String
version_num = VERSION