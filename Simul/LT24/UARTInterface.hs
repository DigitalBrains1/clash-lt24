module Simul.LT24.UARTInterface
       ( module Simul.LT24.UARTInterface
       , module Simul.Common
       , module LT24.UARTInterface
       , module CLaSH.Prelude
       ) where

import CLaSH.Prelude

import Simul.Common
import LT24.UARTInterface
import qualified LT24.LT24 as LT24

simulateCommandIf :: [((Bool, Unsigned 8), Bool, Bool, Bool, Unsigned 16)]
                  -> [(Bool, Unsigned 8, LT24.Action, Unsigned 16)]

simulateCommandIf = simulate ( pack . commandIf . unpack)
                  . (++ repeat ((False, 0), False, True, True, 0))

simulatePassCommand :: [(Unsigned 8, Unsigned 16, Bool, Bool)]
                    -> [(LT24.Action, Unsigned 16)]

simulatePassCommand = simulate ( pack
                               . (passCommand <^> (PCIdle, LT24.NOP, 0))
                               . unpack)
                    . (++ repeat (0, 0, False, True))
