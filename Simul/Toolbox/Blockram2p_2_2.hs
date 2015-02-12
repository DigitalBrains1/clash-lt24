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

blockram2p :: SNat 12
           -> SNat 12
           -> SNat 2
           -> SNat 2
           -> SignalP ( Unsigned 12, Unsigned 2, Bool, Unsigned 12
                      , Unsigned 2, Bool)
           -> SignalP (Unsigned 2, Unsigned 2)

blockram2p aaw baw aw bw i = oD
    where
        iD = (unpack . register (0, 0, False, 0, 0, False) . pack) i
        o = (blockram2p' <^> vcopyI 0) i
        oD = (unpack . register (0, 0) . pack) o

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
