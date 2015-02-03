{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Simul.LT24.Init
       ( module Simul.LT24.Init
       , module Simul.Common
       , module LT24.Init
       , module LT24
       ) where

import Simul.Common

import qualified LT24.LT24 as LT24
import LT24.Init

simulateInitLt24 :: [(Bool, LT24.Action, Unsigned 16)]
                 -> [(LT24.Action, Unsigned 16, Bool)]

simulateInitLt24 = simulate (pack . initLt24 . unpack)
