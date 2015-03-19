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

module Simul.Toolbox.Blockram2p_2_2 where

import CLaSH.Prelude
import Control.Applicative
import Debug.Trace

import Toolbox.Misc

-- Simulation model for `blockram2p d12 d12 d2 d2`
blockram2p :: SNat 12
           -> SNat 12
           -> SNat 2
           -> SNat 2
           -> SignalP ( Unsigned 12, Unsigned 2, Bool, Unsigned 12
                      , Unsigned 2, Bool)
           -> SignalP (Unsigned 2, Unsigned 2)

{-
 - While the simulation model from Blockram2p_2_16 in this directory would also
 - work here, it is very slow. The case where both ports have the same width
 - can be greatly simplified, giving a very significant speed boost, especially
 - in interpreted mode where the general simulation model is painfully slow.
 -
 - Here, we just do the registers. `blockram2p'` does the actual memory access.
 -}
blockram2p aaw baw aw bw i = oD
    where
        iD = (unpack . register (0, 0, False, 0, 0, False) . pack) i
        o = (blockram2p' <^> vcopyI 0) i
        oD = (unpack . register (0, 0) . pack) o

{-
 - A rather trivial implementation of a memory, but with two ports. See
 - Blockram2p_2_16 for a discussion of write conflicts.
 -}
blockram2p' :: Vec 4096 (Unsigned 2)
            -> (Unsigned 12, Unsigned 2, Bool, Unsigned 12, Unsigned 2, Bool)
            -> (Vec 4096 (Unsigned 2), (Unsigned 2, Unsigned 2))
blockram2p' s (aAddr, aDIn, aWrEn, bAddr, bDIn, bWrEn)
    = (s', (qA, qB))
    where
        sa' | aWrEn     = vreplace s aAddr aDIn
            | otherwise = s
        s'  | not bWrEn = sa'
            | aWrEn && overlap = error ($(showCodeLoc)
                                        ++ " blockram2p'': Write conflict")
            | otherwise = vreplace sa' bAddr bDIn
        qA = s!aAddr
        qB = s!bAddr
        overlap = aAddr == bAddr
