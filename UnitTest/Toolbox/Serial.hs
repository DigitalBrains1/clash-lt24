{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
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

module UnitTest.Toolbox.Serial
       ( module UnitTest.Toolbox.Serial
       , module Toolbox.Serial
       ) where

import Control.Applicative
import CLaSH.Prelude
import qualified Toolbox.ClockScale as CS
import qualified Toolbox.FIFO as FIFO
import Toolbox.Serial
import Toolbox.FClk

hallo = $(v [72 :: Unsigned 8,97,108,108,111,13,10])

halloRepeater :: Unsigned 3 -> Bool -> (Unsigned 3, (Bool, Unsigned 8))
halloRepeater s ck = (s',(ld, d))
    where
        s' = if ck then
               if s == 0 then
                 6
               else
                 s - 1
             else
               s
        ld = ck
        d  = hallo!s

-- Simply send "Hallo<CR><LF>" over and over on TxD
halloTransmitter = txd
    where
        (ld, d)            = (halloRepeater <^> 6) done
        sTick              = ($(CS.staticAvgRate fClk 115200) <^> 1)
                               stCmd
        (stCmd, done, txd) = output (sTick, ld, d)

-- Same, but using a FIFO
halloTransmitterFIFO = txd
    where
        (full, empty, _, dout) = (    FIFO.fifo
                                  <^> (0, 0, vcopy (snat :: SNat 1)
                                                   (0 :: Unsigned 8))
                                 ) (din, wrt, rd)
        (wrt, din)             = (halloRepeater <^> 6) (fmap not full)
        (rd, stCmd, txd)       = outputFIFO (sTick, empty, dout)
        sTick                  = ($(CS.staticAvgRate fClk 115200) <^> 1)
                                   stCmd

{- Reads characters from RxD and echoes them on TxD, but bit 5 is inverted.
 - This swaps case on alphabetics
 -}
echoSwapCase rxd = txd
    where
        tTick = ($(CS.staticAvgRate fClk 115200) <^> 1)
                  tScaleCmd
        (tScaleCmd, _, txd) = output (tTick, dValid, swappedD)
        rTick = ($(CS.staticAvgRate fClk (16*115200)) <^> 1)
                  (signal CS.Run)
        (r, dValid) = input (rTick, rxd)
        (frameErr, dIn) = unpack r
        swappedD = (xor 32) <$> dIn
