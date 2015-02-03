{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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

