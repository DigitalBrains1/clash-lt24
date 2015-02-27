{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module LT24.Timing where

import CLaSH.Prelude
import qualified Toolbox.ClockScale as CS
import Toolbox.FClk

tResetL = 10e-6
tResetH = 120e-3
tWriteL = 355e-9
tWriteH = 355e-9
tReadLFM = 355e-9
tReadLID = 355e-9
tReadH = 355e-9


wResetL = CS.ticksMinPeriod fClk tResetL
wResetH = CS.ticksMinPeriod fClk tResetH
wWriteL = CS.ticksMinPeriod fClk tWriteL + 4
wWriteH = CS.ticksMinPeriod fClk tWriteH + 4
wReadLFM = CS.ticksMinPeriod fClk tReadLFM + 4
wReadLID = CS.ticksMinPeriod fClk tReadLID + 4
wReadH = CS.ticksMinPeriod fClk tReadH + 4

wLargest = wResetH
