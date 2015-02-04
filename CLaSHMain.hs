module CLaSHMain where

import CLaSH.Prelude

import qualified LT24.UARTInterface as UARTInterface
import Toolbox.Blockram2p

--topEntity = UARTInterface.intfInited
topEntity = blockram2p d12 d9 d2 d16
