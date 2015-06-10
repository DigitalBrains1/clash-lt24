{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-
 - Copyright (c) 2012-2015, University of Twente
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
 -
 - This file is based on examples/Fifo.hs from the CλaSH source code.
 -}
module Toolbox.FIFO
       ( Pntr
       , fifo
       ) where

import CLaSH.Prelude
import Control.Applicative

flowError = False

type Pntr n = Unsigned (n + 1)

{-
 - This is the same FIFO implementation as from the CλaSH examples, with an
 - added output: the number of elements in the FIFO can be examined. This could
 - be useful for Dataflow elements that need multiple tokens to proceed.
 -}
fifo :: forall n e . (KnownNat n, KnownNat (n+1), KnownNat (2^n))
     => (Pntr n, Pntr n, Vec (2^n) e)
     -> (e, Bool, Bool)
     -> ((Pntr n,Pntr n,Vec (2^n) e),(Bool,Bool,Pntr n, e))

fifo (rpntr, wpntr, elms) (datain, wrt, rd)
    = ((rpntr', wpntr', elms'),(full, empty, flength, dataout))
  where
    wpntr' | wrt && not full          = wpntr + 1
           | wrt && full && flowError = error "FIFOn Overflow!"
           | otherwise                = wpntr
    rpntr' | rd && not empty          = rpntr + 1
           | rd && empty && flowError = error "FIFOn Underflow!"
           | otherwise                = rpntr

    mask = maxBound `shiftR` 1
    wind = wpntr .&. mask
    rind = rpntr .&. mask

    elms' | wrt       = vreplace elms wind datain
          | otherwise = elms

    n = fromInteger $ natVal rpntr - 1

    flength = wpntr - rpntr
    empty   = flength == 0
    full    = flength == bit n

    dataout = elms!rind

