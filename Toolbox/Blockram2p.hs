{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Toolbox.Blockram2p where

import CLaSH.Prelude
import Control.Applicative

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

blockram2p aaw baw aw bw (aAddr, aDin, aWrEn, bAddr, bDin, bWrEn) = (qA, qB)
    where
        (qAB, qBB) = withSNat (withSNat blockram2p') aaw baw aw bw
                       (aAddrB) (aDinB) aWrEnB (bAddrB) (bDinB) bWrEnB
        qA = fromBV <$> qAB
        qB = fromBV <$> qBB
        aAddrB = toBV <$> aAddr
        aDinB = toBV <$> aDin
        aWrEnB = (\b -> if b then H else L) <$> aWrEn
        bAddrB = toBV <$> bAddr
        bDinB = toBV <$> bDin
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

-- TODO: Implement for simulation
{-# NOINLINE blockram2p' #-}
blockram2p' an bn aaw baw aw bw aAddrB aDinB aWrEnB bAddrB bDinB bWrEnB
    = (signal (vcopyI L), signal (vcopyI L))

