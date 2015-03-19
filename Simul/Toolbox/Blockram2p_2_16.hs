{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

module Simul.Toolbox.Blockram2p_2_16 where

import CLaSH.Prelude
import Control.Applicative
import Debug.Trace

import Toolbox.Misc


-- Simulation model for `blockram2p d12 d9 d2 d16`
blockram2p :: SNat 12
           -> SNat 9
           -> SNat 2
           -> SNat 16
           -> SignalP ( Unsigned 12, Unsigned 2, Bool, Unsigned 9
                      , Unsigned 16, Bool)
           -> SignalP (Unsigned 2, Unsigned 16)

-- Nearly identical to Toolbox.blockram2p; the only difference is that
-- blockram2p' no longer takes the first two arguments `an` and `bn`.
blockram2p aaw baw aw bw (aAddr, aDIn, aWrEn, bAddr, bDIn, bWrEn) = (qA, qB)
    where
        (qAB, qBB) = blockram2p' aaw baw aw bw
                       (aAddrB) (aDInB) aWrEnB (bAddrB) (bDInB) bWrEnB
        qA = fromBV <$> qAB
        qB = fromBV <$> qBB
        aAddrB = toBV <$> aAddr
        aDInB = toBV <$> aDIn
        aWrEnB = (\b -> if b then H else L) <$> aWrEn
        bAddrB = toBV <$> bAddr
        bDInB = toBV <$> bDIn
        bWrEnB = (\b -> if b then H else L) <$> bWrEn

{-
 - This should be the simulation model for the general case, but I could not
 - get the type proven correct.
 -
 - It is also quite slow in simulation.
 -
 - Registers are in this function, and the bitvectors are converted back to
 - Unsigneds and Bools. `blockram2p''` does the actual memory
 - access.
 -}
blockram2p' aaw baw aw bw aAddrB aDInB aWrEnB bAddrB bDInB bWrEnB
    = oD
    where
        aAddr = fromBV <$> aAddrB
        aWrEn = (== H) <$> aWrEnB
        bAddr = fromBV <$> bAddrB
        bWrEn = (== H) <$> bWrEnB
        iD = ( unpack
             . register (0, vcopyI L, False, 0, vcopyI L, False)
             . pack) (aAddr, aDInB, aWrEn, bAddr, bDInB, bWrEn)
        oD = ( unpack . register (vcopyI L, vcopyI L) . pack) o
        o = (blockram2p'' aaw baw aw bw <^> vcopyI L) iD

{-
 - Memory is held as a vector of individual bits. These are then aggregrated
 - into the word size used by the two ports.
 -
 - In the FPGA, a write conflict gives undefined behaviour. This simulation
 - model is defined in such a way that port B data would overwrite port A data
 - for the part where there is an overlap; but instead of doing that, an error
 - is thrown.
 -
 - Usually, a write conflict is a design error. Even if it is carefully
 - constructed not to be, the simulation yields a different result (i.e.,
 - deterministic) than the FPGA (i.e., undefined), so throwing an error in the
 - simulation is definitely the safest thing to do.
 -}
blockram2p'' :: SNat 12
             -> SNat 9
             -> SNat 2
             -> SNat 16
             -> Vec 8192 Bit
             -> (Unsigned 12, Vec 2 Bit, Bool, Unsigned 9, Vec 16 Bit, Bool)
             -> (Vec 8192 Bit, (Vec 2 Bit, Vec 16 Bit))
blockram2p'' aaw baw aw bw s (aAddr, aDIn, aWrEn, bAddr, bDIn, bWrEn)
    = (s', (qA, qB))
    where
        sa' | aWrEn     = vconcat $ vreplace (vunconcatI s) aAddr aDIn
            | otherwise = s
        s'  | not bWrEn = sa'
            | aWrEn && overlap = error ($(showCodeLoc)
                                        ++ " blockram2p'': Write conflict")
            | otherwise = vconcat $ vreplace (vunconcatI sa') bAddr bDIn
        qA = (vunconcatI s)!aAddr
        qB = (vunconcatI s)!bAddr
        astart = (fromIntegral aAddr) * awv
        aend = astart + awv
        bstart = (fromIntegral bAddr) * bwv
        bend = bstart + bwv
        overlap =    (bstart >= astart && bstart < aend)
                  || (astart >= bstart && astart < bend)
        awv = fromInteger $ natVal aw
        bwv = fromInteger $ natVal bw
