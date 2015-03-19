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

module UnitTest.LT24.LT24.LT24Top where

import CLaSH.Prelude
import Control.Applicative

import qualified LT24.LT24 as LT24

-- Use lt24 as a toplevel entity, for simulation

topEntity i = o
    where
        o = ((combineOutput <$>) . pack)
              (ready, dout, lcdOn, csx, resx, dcx, wrx, rdx, ltdout, oe)
        (action, din, ltdin) = (unpack . (splitInput <$>)) i
        (ready, dout, lcdOn, csx, resx, dcx, wrx, rdx, ltdout, oe)
            = LT24.lt24 (action, din, ltdin)

combineOutput (ready, dout, lcdOn, csx, resx, dcx, wrx, rdx, ltdout, oe)
    = (toBV ready <++> toBV dout
       <++> (lcdOn :> csx :> resx :> dcx :> wrx :> rdx :> Nil)
       <++> toBV ltdout) <: oe

splitInput :: Vec 35 Bit
           -> (LT24.Action, Unsigned 16, Unsigned 16)

splitInput i = (action, din, ltdin)
    where
        actionN = fromBV (vtakeI i) :: Unsigned 3
        din = fromBV (vselect d3 d1 d16 i) :: Unsigned 16
        ltdin = fromBV (vdropI i) :: Unsigned 16

        action = case actionN of
                   1 -> LT24.Reset
                   2 -> LT24.Command
                   3 -> LT24.Write
                   4 -> LT24.ReadFM
                   5 -> LT24.ReadID
                   _ -> LT24.NOP
