module Main where

import CLaSH.Prelude

import qualified LT24.UARTInterface as UARTInterface

--topEntity = UARTInterface.intfInited
import UnitTest.LT24.Framebuffer.BouncyBall (topEntity)

import Simul.LT24.DummyLT24

main = putStrLn $ show $ onlyTrace 1000000 topEntity
