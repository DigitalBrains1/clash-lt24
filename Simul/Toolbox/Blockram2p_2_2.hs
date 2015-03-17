{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
blockram2p' s (aAddr, aDin, aWrEn, bAddr, bDin, bWrEn)
    = (s', (qA, qB))
    where
        sa' | aWrEn     = vreplace s aAddr aDin
            | otherwise = s
        s'  | not bWrEn = sa'
            | aWrEn && overlap = error ($(showCodeLoc)
                                        ++ " blockram2p'': Write conflict")
            | otherwise = vreplace sa' bAddr bDin
        qA = s!aAddr
        qB = s!bAddr
        overlap = aAddr == bAddr
