module Simul.LT24.LT24
       ( module Simul.LT24.LT24
       , module Simul.Common
       , module LT24.LT24
       , module CLaSH.Prelude
       ) where

import CLaSH.Prelude

import Simul.Common
import LT24.LT24

simulateLT24 :: [(Action, Unsigned 16, Unsigned 16)]
             -> [ (Bool, Unsigned 16, Bit, Bit, Bit, Bit, Bit, Unsigned 16
                 , Bit)]

simulateLT24 = simulate (pack . lt24 . unpack)
             . (++ repeat (NOP, 0, 0))

