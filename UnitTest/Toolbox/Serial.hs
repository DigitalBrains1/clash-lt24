{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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
