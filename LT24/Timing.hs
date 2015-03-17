{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module LT24.Timing where

import CLaSH.Prelude
import qualified Toolbox.ClockScale as CS
import Toolbox.FClk

{-
 - All numbers come from page 238 of the datasheet (with the exception of the
 - reset timing, which is on page 230)
 -
 - t...L : signal low time
 - t...H : signal high time
 - t...C : cycle time; the sum of L and H times should be at least this much. We
 -         do this here by extending the H wait period if necessary.
 -
 - There are two timings for reads: FM and ID. These letters are appended to
 - the name of the constant. See LT24.LT24 for some more discussion of this.
 -}
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

{-
 - The number of wait states that need to be introduced to respect the timings
 - above. This is expressed in clock ticks. The actual number of ticks each
 - phase takes is one more than the number of wait states (hence the extra
 - minus one term in the H phase).
 -}
wResetL = ticks tResetL
wResetH = ticks tResetH
wWriteL = ticks tWriteL
wWriteH = max (ticks tWriteH) (ticks tWriteC - wWriteL - 1)
{-
 - Because of the register that is in front of the ltdin signal, the data is
 - available after at least the access time plus one additional clock tick to
 - get through the register. So the minimum low time is the greater of either
 - tREADAFM + 1 or tReadLFM. Same for the ID timings.
 -}
wReadLFM = max (ticks tReadLFM) (ticks tReadAFM + 1)
wReadLID = max (ticks tReadLID) (ticks tReadAID + 1)
wReadH = foldl1 max [ ticks tReadCFM - wReadLFM - 1
                    , ticks tReadCID - wReadLID - 1, ticks tReadH ]

-- The largest number, for sizing the register holding the counter.
wLargest = wResetH
