{-# LANGUAGE DataKinds, TemplateHaskell, TypeFamilies #-}

module Toolbox.Serial where

import CLaSH.Prelude
import Control.Applicative
import qualified Toolbox.ClockScale as CS
import qualified Toolbox.FIFO as FIFO

{-
 - Serial data output routine
 -
 - This routine will send bytes over a serial line according to the
 - asynchronous start/stop protocol. It only supports the dataformat 8N1 (8
 - databits, no parity, 1 stopbit).
 -
 - Inputs: (ck, ld, din):
 -      - ck: This should be the output of a ClockScaler that scales to the
 -        bitclock or baud rate of the serial line. It is True once every
 -        period of the baud rate.
 -      - ld: True if there's a new piece of data on din. Should only be
 -        asserted when output "done" is also true.
 -      - din: 8-bit word to send out
 -
 - Outputs: (scaler, done, txd):
 -      - scaler: a ClockScale.State that controls the ClockScaler that does
 -        the pacing for this serial output.
 -      - done: True when new data can be accepted because the previous data
 -        has been sent.
 -      - txd: The actual output line where data is sent using asynchronous
 -        start/stop.
 -}

output :: (Signal Bool, Signal Bool, Signal (Unsigned 8))
       -> (Signal CS.State, Signal Bool, Signal Bit)

output = output' <^> (vcopyI L)

output' :: Vec 10 Bit
        -> (Bool, Bool, Unsigned 8)
        -> (Vec 10 Bit, (CS.State, Bool, Bit))

output' shifter (ck, ld, din) = (shifter', (scaler, done, txd))
    where
        shifter' = if ld then
                     H :> (toBV din <: L)
                   else if ck then
                     L +>> shifter
                   else
                     shifter
        (scaler, done) = if shifter
                            == $(v [L, L, L, L, L, L, L, L, L, L]) then
                           (CS.Clear, True)
                         else
                           (CS.Run  , False)
        txd            = if done then
                           H
                         else
                           vlast shifter

{-
 - A variant of "output" that reads its data from a FIFO
 -}

outputFIFO (ck, empty, din) = (ld, scaler, txd)
    where
        (scaler, done, txd) = output (ck, ld, din)
        ld                  = (\(e, d) -> not e && d) <$> pack (empty, done)

data State = WaitStart | Sample (Unsigned 2) | WaitHigh
    deriving Show

{- Asynchronous start/stop receiver
 -
 - Frame format 8N1 only
 -
 - Inputs (cTick, dIn):
 -      - cTick: ticks from a continuously running ClockScaler that runs at 16
 -        times the baud rate of the incoming serial line. This divides one
 -        bitperiod in 16 equal parts which are used for the sampling of the
 -        line.
 -      - dIn: the pin of the FPGA the serial data is to be read from.
 -
 - Outputs ((frameErr, dOut), dValid):
 -      - dValid: True if (frameErr, dOut) is valid
 -      - frameErr: True if the stopbit was not "mark" (i.e., it was "space").
 -        This means something went wrong in reception, so the data might be
 -        corrupted as well.
 -      - dOut: The received databit
 -
 - Majority sampling principle: 3 samples are taken around the center of the
 - databit, a majority counter counts the number of high samples. If this
 - counter is 2 or 3, the most significant bit is high, otherwise, it is low.
 - In other words, the most significant bit is the outcome of the majority
 - vote.
 -}

input (cTick, dIn) = (input' <^> initInput) (cTick, dInS)
    where
        dInS = register H $ register H dIn

input' s@(mode, wait, run, shift, sample) (cTick, dIn)
    = (s', ((frameErr, dOut), dValid))
    where
        s'@(mode', wait', run', shift', sample') = input'' s cTick dIn
        --s' = (mode', wait', run', shift', sample')
        frameErr = vhead shift' == L
        dOut = fromBV $ vtail shift' :: Unsigned 8
        dValid = run && (not run')

input'' :: (State, Unsigned 4, Bool, Vec 9 Bit, Unsigned 2)
       -> Bool
       -> Bit
       -> (State, Unsigned 4, Bool, Vec 9 Bit, Unsigned 2)

input'' s                                         False _
    = s

input'' s@(WaitStart, 0   , _    , shift, sample) _     H
    = s

input''   (WaitStart, 0   , _    , shift, sample) _     L
    = (Sample 1 , 6, False, shift, 0)

input''   (Sample n , 0   , run  , shift, sample) _     dIn
    = case (n, run, vlast shift) of
       (3, False, _) -> if majority == H then
                          waitEdge -- Not a startbit, but a glitch
                        else
                          (Sample 1, 13, True, H :> vcopyI L, 0)
       (3, True  , H) -> waitEdge
       (3, True  , L) -> (Sample 1, 13, True, shift', 0)
       (_, _     , _) -> (Sample (n + 1), 0, run, shift, sample')
    where
        sample' = if dIn == H then
                    sample + 1
                  else
                    sample
        majority = vhead $ toBV sample'
        shift' = majority +>> shift

        waitEdge = if dIn == H then
                     (WaitStart, 0, False, shift', sample)
                   else
                     (WaitHigh , 0, False, shift', sample)

input'' s@(WaitHigh , 0   , run  , shift, sample) _     L
    = s

input'' s@(WaitHigh , 0   , run  , shift, sample) _     H
    = (WaitStart, 0, run, shift, sample)

input''   (mode     , wait, run  , shift, sample) _     _
    = (mode, wait - 1, run, shift, sample)

initInput :: (State, Unsigned 4, Bool, Vec 9 Bit, Unsigned 2)

initInput = (WaitStart, 0, False, vcopyI L, 0)

