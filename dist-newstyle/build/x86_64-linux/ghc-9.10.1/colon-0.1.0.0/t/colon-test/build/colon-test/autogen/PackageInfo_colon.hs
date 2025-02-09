{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_colon (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "colon"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Custom interpreter for Colon-like language"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
