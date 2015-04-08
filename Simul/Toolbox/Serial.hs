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

module Simul.Toolbox.Serial
       ( module Simul.Toolbox.Serial
       , module Simul.Common
       , module Toolbox.Serial
       ) where

import CLaSH.Prelude
import Control.Applicative
import CLaSH.Prelude
import Data.Char
import qualified Toolbox.ClockScale as CS
import Toolbox.Serial

import Simul.Common

-- Create a test input to send an ASCII 'A' and a 'B' after some delay
outputTestInput delay1 delay2 =    take delay1 (repeat (False, undefined))
                                ++ [ (True, 65) ] -- 01000001b
                                ++ take delay2
                                     (repeat (False, undefined))
                                ++ [ (True, 66) ] -- 01000010b
                                ++ repeat (False, undefined)

-- An instance of output running with a clock divider of `scale`
testOutputC scale (ld, din) = txd
    where
        tick = (CS.avg <^> (0 :: Unsigned 27))
                 (1, signal scale, scale_cmd)
        (scale_cmd, done, txd) = output (tick, ld, din)

-- Simulate testOutputC
simulateOutput :: Unsigned 26 -> [(Bool, Unsigned 8)]
               -> [Bit]

simulateOutput scale = simulate (testOutputC scale . unpack)

{- Simulate Serial.input with a non-stop input clock, so 16 input samples per
 - bit
 -}
simulateInput :: [Bit]
              -> [( (State, Unsigned 4, Bool, Vec 9 Bit, Unsigned 2)
                 , ((Bool, Unsigned 8), Bool))]

simulateInput =  simulate (pack . (testC <^> initInput) . unpack)
              . map (\b -> (True, b))
    where
        testC s i = let (s', o) = input' s i in (s', (s', o))


-- Create the input for simulateInput from an ASCII string
stringToTestInput :: String -> [Bit]

stringToTestInput =  concat . concat
                   . map (  map (take 16 . repeat) . (++ [H]) . (L:) . toList
                          . vreverse . toBV
                          . (fromIntegral :: Int -> Unsigned 8) . ord)

-- Pretty print the output from simulateInput
prettySI :: [( (State, Unsigned 4, Bool, Vec 9 Bit, Unsigned 2)
            , ((Bool, Unsigned 8), Bool))]
         -> IO ()

prettySI = pretty . map vec2string
    where
        vec2string ((mode, count, run, shift, sample), o)
            = ((mode, count, run, show shift, sample), o)

