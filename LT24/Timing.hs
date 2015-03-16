{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module LT24.Timing where

import CLaSH.Prelude
import qualified Toolbox.ClockScale as CS
import Toolbox.FClk

tResetL = 10e-6
tResetH = 120e-3
tWriteL = 15e-9
tWriteH = 15e-9
tWriteC = 66e-9
tReadLFM = 355e-9
tReadLID = 45e-9
tReadAFM = 340e-9  -- Read access time (FM)
tReadAID = 40e-9   -- Read access time (ID)
tReadH = 90e-9
tReadCFM = 450e-9  -- Read cycle time (FM)
tReadCID = 160e-9  -- Read cycle time (ID)

ticks = CS.ticksMinPeriod fClk

wResetL = ticks tResetL
wResetH = ticks tResetH
wWriteL = ticks tWriteL
wWriteH = max (ticks tWriteH) (ticks tWriteC - wWriteL - 1)
wReadLFM = max (ticks tReadLFM) (ticks tReadAFM + 1)
wReadLID = max (ticks tReadLID) (ticks tReadAID + 1)
wReadH = foldl1 max [ ticks tReadCFM - wReadLFM - 1
                    , ticks tReadCID - wReadLID - 1, ticks tReadH ]

wLargest = wResetH
