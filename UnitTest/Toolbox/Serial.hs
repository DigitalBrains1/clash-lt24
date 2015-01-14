{-# LANGUAGE DataKinds, TemplateHaskell, TypeFamilies #-}

module UnitTest.Toolbox.Serial
       ( module UnitTest.Toolbox.Serial
       , module UnitTest.Common
       , module Toolbox.Serial
       ) where

import Control.Applicative
import CLaSH.Prelude
import Data.Char
import qualified Toolbox.ClockScale as CS
import qualified Toolbox.FIFO as FIFO
import Toolbox.Serial
import UnitTest.Common

outputTestInput delay1 delay2 =    take delay1 (repeat (False, undefined))
                                ++ [ (True, 65) ] -- 01000001b
                                ++ take delay2
                                     (repeat (False, undefined))
                                ++ [ (True, 66) ] -- 01000010b
                                ++ repeat (False, undefined)

testOutputC scale (ld, din) = txd
    where
        tick = (CS.dynamic <^> (0 :: Unsigned 27))
                 (1, scale, scale_cmd)
        (scale_cmd, done, txd) = output (tick, ld, din)

testOutput :: Int -> Signal (Unsigned 26) -> [(Bool, Unsigned 8)]
                  -> [Bit]

testOutput time scale =   simulate (testOutputC scale . unpack)
                        . (take time)


-- The name "testInput" is reserved, so add a C for Component
testInputC :: [Bit]
          -> [( (State, Unsigned 4, Bool, Vec 9 Bit, Unsigned 2)
              , ((Bool, Unsigned 8), Bool))]

testInputC =  simulate (pack . (testC <^> initInput) . unpack)
           . map (\b -> (True, b))
    where
        testC s i = let (s', o) = input' s i in (s', (s', o))


stringToTestInput :: String -> [Bit]

stringToTestInput =  concat . concat
                   . map (  map (take 16 . repeat) . (++ [H]) . (L:) . toList
                          . vreverse . toBV
                          . (fromIntegral :: Int -> Unsigned 8) . ord)

-- Actual designs for synthesis

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

halloTransmitter = txd
    where
        (ld, d)            = (halloRepeater <^> 6) done
        sTick              = ($(CS.staticRate fClk 115200) <^> 1)
                               stCmd
        (stCmd, done, txd) = output (sTick, ld, d)

halloTransmitterFIFO = txd
    where
        (full, empty, _, dout) = (    FIFO.fifo
                                  <^> (0, 0, vcopy (snat :: SNat 1)
                                                   (0 :: Unsigned 8))
                                 ) (din, wrt, rd)
        (wrt, din)             = (halloRepeater <^> 6) (fmap not full)
        (rd, stCmd, txd)       = outputFIFO (sTick, empty, dout)
        sTick                  = ($(CS.staticRate fClk 115200) <^> 1)
                                   stCmd

echoSwapCase rxd = txd
    where
        rxdS = register H $ register H rxd
        tTick = ($(CS.staticRate fClk 115200) <^> 1)
                  tScaleCmd
        (tScaleCmd, _, txd) = output (tTick, dValid, swappedD)
        rTick = ($(CS.staticRate fClk (16*115200)) <^> 1)
                  (signal CS.Run)
        (r, dValid) = input (rTick, rxdS)
        (frameErr, dIn) = unpack r
        swappedD = (xor 32) <$> dIn
