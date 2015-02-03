{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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

