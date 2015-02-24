{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Simul.Toolbox.Blockram2p where

import CLaSH.Prelude
import Control.Applicative
import Debug.Trace

import Toolbox.Misc


blockram2p :: SNat 12
           -> SNat 9
           -> SNat 2
           -> SNat 16
           -> SignalP ( Unsigned 12, Unsigned 2, Bool, Unsigned 9
                      , Unsigned 16, Bool)
           -> SignalP (Unsigned 2, Unsigned 16)

{-
 - The addressing behaviour of the M9K blockram in an Altera Cyclone IV E is
 - odd when using mixed port widths. See commit 25ef171 and pixelLane from
 - LT24/Framebuffer.hs. The implementation below is (probably) what I had
 - expected it to do. However, with a 16-bit and a 2-bit port, the offset of
 - the 2 bits in a 16-bit word is off by one: the lowest numbered address is at
 - the top of the 16-bit word, the next at the bottom and then numbering
 - logically.
 -}
blockram2p = error ($(showCodeLoc)
                    ++ "Simulation of mixed-width blockram is incorrect")

--blockram2p aaw baw aw bw (aAddr, aDin, aWrEn, bAddr, bDin, bWrEn) = (qA, qB)
--    where
--        (qAB, qBB) = blockram2p' aaw baw aw bw
--                       (aAddrB) (aDinB) aWrEnB (bAddrB) (bDinB) bWrEnB
--        qA = fromBV <$> qAB
--        qB = fromBV <$> qBB
--        aAddrB = toBV <$> aAddr
--        aDinB = toBV <$> aDin
--        aWrEnB = (\b -> if b then H else L) <$> aWrEn
--        bAddrB = toBV <$> bAddr
--        bDinB = toBV <$> bDin
--        bWrEnB = (\b -> if b then H else L) <$> bWrEn

blockram2p' aaw baw aw bw aAddrB aDinB aWrEnB bAddrB bDinB bWrEnB
    = oD
    where
        aAddr = fromBV <$> aAddrB
        aWrEn = (== H) <$> aWrEnB
        bAddr = fromBV <$> bAddrB
        bWrEn = (== H) <$> bWrEnB
        iD = ( unpack
             . register (0, vcopyI L, False, 0, vcopyI L, False)
             . pack) (aAddr, aDinB, aWrEn, bAddr, bDinB, bWrEn)
        oD = ( unpack . register (vcopyI L, vcopyI L) . pack) o
        o = (blockram2p'' aaw baw aw bw <^> vcopyI L) iD

blockram2p'' :: SNat 12
             -> SNat 9
             -> SNat 2
             -> SNat 16
             -> Vec 8192 Bit
             -> (Unsigned 12, Vec 2 Bit, Bool, Unsigned 9, Vec 16 Bit, Bool)
             -> (Vec 8192 Bit, (Vec 2 Bit, Vec 16 Bit))
blockram2p'' aaw baw aw bw s (aAddr, aDin, aWrEn, bAddr, bDin, bWrEn)
    = (s', (qA, qB))
    where
        sa' | aWrEn     = vconcat $ vreplace (vunconcatI s) aAddr aDin
            | otherwise = s
        s'  | not bWrEn = sa'
            | aWrEn && overlap = error ($(showCodeLoc)
                                        ++ " blockram2p'': Write conflict")
            | otherwise = vconcat $ vreplace (vunconcatI sa') bAddr bDin
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
