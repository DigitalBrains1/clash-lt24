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

module Toolbox.Blockram2p where

import CLaSH.Prelude
import Control.Applicative

{-
 - Instantiate a black box dual port blockram with the following parameters:
 -
 - `aaw` - Address bus width of port A
 - `baw` - Address bus width of port B
 - `aw`  - Data width of port A
 - `bw`  - Data width of port B
 -
 - Both ports need to address the same amount of memory. So if you would, for
 - instance, have 4096 words of 4 bits on port A, and port B uses 16-bit data,
 - port B would need to address 1024 words. The type checker will error if this
 - constraint is not satisfied.
 -
 - The inputs are:
 - `aAddr` - Address bus port A
 - `aDIn`  - Data in port A
 - `aWrEn` - Write enable for port A (read is always enabled)
 - ... and then similar for port B
 -
 - The outputs are:
 - `qA` - Data out port A
 - `qB` - Data out port B
 -
 - The blockram has registers preceding and registers following the actual
 - blockram. During a write, the old data from that address is on the output
 - port; this goes for both same-port and mixed-port access.
 -
 - Because of the registers, if you offer an address in clock cycle 0, you see
 - the data in clock cycle 2. Both reads and writes can be fully pipelined.
 - Write conflicts are unhandled and have undefined results.
 -
 - The current implementation instantiates an Altera M9K block (such as is
 - present in the Altera DE0-Nano board).  For more details, look at the VHDL
 - that the blackbox instantiates and consult the Altera documentation. If you
 - swap out the blackbox for a different implementation, watch out for the
 - precise settings with regard to read-during-write and such details.
 -
 - `blockram2p` simply converts between Unsigneds and Bools on the one hand and
 - Vecs n Bit on the other.
 -}
blockram2p :: forall aaw baw aw bw .
              ( KnownNat (2 ^ aaw), KnownNat (2 ^ baw), KnownNat aaw
              , KnownNat baw, KnownNat aw, KnownNat bw
              , KnownNat ((2 ^ aaw) * aw)
              , ((2 ^ aaw) * aw) ~ ((2 ^ baw) * bw))
           => SNat aaw
           -> SNat baw
           -> SNat aw
           -> SNat bw
           -> SignalP ( Unsigned aaw, Unsigned aw, Bool, Unsigned baw
                      , Unsigned bw, Bool)
           -> SignalP (Unsigned aw, Unsigned bw)

blockram2p aaw baw aw bw (aAddr, aDIn, aWrEn, bAddr, bDIn, bWrEn) = (qA, qB)
    where
        (qAB, qBB) = withSNat (withSNat blockram2p') aaw baw aw bw
                       (aAddrB) (aDInB) aWrEnB (bAddrB) (bDInB) bWrEnB
        qA = fromBV <$> qAB
        qB = fromBV <$> qBB
        aAddrB = toBV <$> aAddr
        aDInB = toBV <$> aDIn
        aWrEnB = (\b -> if b then H else L) <$> aWrEn
        bAddrB = toBV <$> bAddr
        bDInB = toBV <$> bDIn
        bWrEnB = (\b -> if b then H else L) <$> bWrEn

blockram2p' :: forall an bn aaw baw aw bw mbw .
               ( KnownNat an, KnownNat bn, KnownNat aaw
               , KnownNat baw, KnownNat aw, KnownNat bw, KnownNat mbw
               , an ~ (2 ^ aaw), bn ~ (2 ^ baw)
               , mbw ~ (an * aw)
               , mbw ~ (bn * bw))
            => SNat an
            -> SNat bn
            -> SNat aaw
            -> SNat baw
            -> SNat aw
            -> SNat bw
            -> Signal (Vec aaw Bit)
            -> Signal (Vec aw Bit)
            -> Signal Bit
            -> Signal (Vec baw Bit)
            -> Signal (Vec bw Bit)
            -> Signal Bit
            -> (Signal (Vec aw Bit), Signal (Vec bw Bit))

-- Simulation proved difficult, as I could not get the type proven correct. So
-- simulation is defined for specific parameters in Simul.Toolbox.
{-# NOINLINE blockram2p' #-}
blockram2p' an bn aaw baw aw bw aAddrB aDInB aWrEnB bAddrB bDInB bWrEnB
    = (signal (vcopyI L), signal (vcopyI L))

