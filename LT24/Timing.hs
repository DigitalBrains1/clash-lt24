{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-
 - Copyright (c) 2015, Peter Lebbing <peter@digitalbrains.com>
 - All rights reserved.
 -
 - Redistribution and use in source and binary forms, with or without
 - modification, are permitted provided that the following conditions are met:
 -
 - 1. Redistributions of source code must retain the above copyright notice,
 - this list of conditions and the following disclaimer.
 -
 - 2. Redistributions in binary form must reproduce the above copyright notice,
 - this list of conditions and the following disclaimer in the documentation
 - and/or other materials provided with the distribution.
 -
 - THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 - AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 - IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 - ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 - LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 - CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 - SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 - INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 - CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 - ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 - POSSIBILITY OF SUCH DAMAGE.
 -}

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
