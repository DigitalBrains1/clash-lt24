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

module Simul.LT24.DummyLT24 where

import CLaSH.Prelude
import Control.Applicative
import Debug.Trace

import LT24.LT24 (Action(..))

strideBusy = 3
strideReady = 3

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
        ready = (lt24' <^> (True, 1, 1)) iD

lt24' :: (Bool, Integer, Integer)
      -> (Action, Unsigned 16, Unsigned 16)
      -> ((Bool, Integer, Integer), Bool)

lt24'   (False, 1, n) i                  = trace "Done"
                                             ((True, strideReady, n), True)
lt24' s@(True , 1, n)    (NOP   , din, _) = (s, True)
lt24'   (True , 1, n)  i@(Write , din, _) = lt24'' n i
lt24'   (True , 1, n)  i@(ReadFM, din, _) = lt24'' n i
lt24'   (True , 1, n)  i@(ReadID, din, _) = lt24'' n i
lt24'   (True , 1, n)    (action, din, _) = trace ( (shows action . (',':)
                                                    . shows din) "")
                                              ((False, strideBusy, 1), False)
lt24'   (r    , w, n)  i                  = ((r, w-1, n), r)

lt24'' n (action, din, _) = trace ( (shows n . (':':) . shows action
                                    . (',':) . shows din) "")
                              ((False, strideBusy, n+1), False)

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
