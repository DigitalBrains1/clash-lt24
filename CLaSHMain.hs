module CLaSHMain
       (topEntity
       ) where

import CLaSH.Prelude

import qualified LT24.UARTInterface as UARTInterface

--topEntity = UARTInterface.intfInited
import UnitTest.LT24.Framebuffer.BouncyBall (topEntity)
