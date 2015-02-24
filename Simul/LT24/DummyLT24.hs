{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Simul.LT24.DummyLT24 where

import CLaSH.Prelude
import Control.Applicative
import Debug.Trace

import LT24.LT24 (Action(..))

stride = 3

lt24 i@(action, din, ltdin)
    = (ready, dout, lcd_on, csx, resx, dcx, wrx, rdx, ltdout, oe)
    where
        dout = signal (0 :: Unsigned 16)
        lcd_on = signal H
        csx = signal L
        resx = signal H
        dcx = signal H
        wrx = signal H
        rdx = signal H
        ltdout = din
        oe = signal L

        iD = (unpack . register (NOP, 0, 0) . pack) i
        ready = (lt24' <^> (0, 1)) iD

lt24' :: (Integer, Integer)
      -> (Action, Unsigned 16, Unsigned 16)
      -> ((Integer, Integer), Bool)
lt24' s@(0, n)    (NOP   , din, _) = (s, True)
lt24'   (0, n)  i@(Write , din, _) = lt24'' n i
lt24'   (0, n)  i@(ReadFM, din, _) = lt24'' n i
lt24'   (0, n)  i@(ReadID, din, _) = lt24'' n i
lt24'   (0, n)    (action, din, _) = trace ( (shows action . (',':)
                                              . shows din) "")
                                              ((stride, 1  ), False)
lt24'   (1, n)    (action, din, _) = trace ("Done") ((0, n), True)
lt24'   (w, n)  i                  = ((w-1, n), False)

lt24'' n  (action, din, _) = trace ( (shows n . (':':) . shows action
                                   . (',':) . shows din) "")
                                   ((stride, n+1), False)

{-
 - If you just want to see trace messages, the following function is helpful
 - from interactive CλaSH. It is assumed that ltdout is visible on the second
 - bit in the output vector of Bit; if the design has the Output Enable on the
 - lowest bit and ltdout on the bits after that, this condition has been met.
 - By monitoring ltdout, and the fact that DummyLT24.lt24 passes din to
 - ltdout, it is assured that din needs to be computed, usually resulting in
 - the evaluation of all display interaction.
 -
 - `t`: number of timesteps to simulate
 - `e`: the CλaSH component to test (f.e., topEntity)
 -}

onlyTrace t e = foldl xor L $ map (vexact d1) $ take t $ simulate e $ repeat
                $ vcopyI L
