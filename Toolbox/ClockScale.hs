{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Toolbox.ClockScale
       ( rateParams
       , State(..)
       , avg
       , max
       , staticAvg
       , staticAvgRate
       , staticMax
       , staticMaxRate
       ) where

import Prelude hiding (max)
import Language.Haskell.TH
import CLaSH.Prelude
import Toolbox.Misc

instance Pack State where
    type SignalP State = Signal State
    pack = id
    unpack = id

{-
 - Compute the multiplier and divisor needed for the clock scaler to scale from
 - the clockrate "from" to ticks with a rate of "to"
 -
 - This is not meant to be compiled to VHDL; instead, use it to create the
 - parameters and use those parameters directly in the design. An example of
 - this is the convenient "staticRate" function in this module that can be
 - instantiated with Template Haskell.
 -}

rateParams :: Integer -> Integer -> (Integer, Integer)
rateParams from to = (m, d)
    where
        common = lcm from to
        m      = common `div` from
        d      = common `div` to


data State = Stop | Run | Clear
    deriving (Eq, Show)

{-
 - Scale a clock frequency as accurately as possible
 -
 - Outputs True once every "tick" of the desired frequency by counting and
 - scaling the system clock.
 -
 - Inputs: m - Multiplier for the system clock d - Divisor for the target clock
 -
 - The counter never goes to 0; it is most accurate when initialised to 1.
 - 
 - Make sure the target clock is lower than the system clock, or you will miss
 - ticks. Not surprising, really :).
 -
 - Also, the degenerate case where the divisor is 1 (or equivalently, the
 - target clock is equal to the system clock) *will not stop* when given the
 - command Stop or Clear. Don't use this configuration; the following would
 - probably be more what you need:
 -
 - tick = (== ClockScale.Run) <$> cmd
 -
 - Note that it is designed to be instantiated with a static divisor and
 - multiplier. Changing them while running will cause glitching.
 -}

avg :: (KnownNat n, KnownNat (n+1))
    => Unsigned (n+1)
    -> (Unsigned n, Unsigned n, State)
    -> (Unsigned (n+1), Bool)

avg s (m, d, cmd) = (s', o)
    where
        sinc = s + resize m
        wrap = s >= resize d
        s'   = if cmd == Clear then
                 1
               else if wrap then
                 sinc - resize d
               else if cmd == Stop then
                 s
               else
                 sinc
        o    = wrap

{-
 - Scale a clock frequency, not exceeding the given bound
 -
 - Where `avg` tries to approximate the desired frequency as closesly as
 - possible and might cause a short pulse to compensate for a long pulse, this
 - implementation makes sure not to exceed the target frequency. Thus it is
 - more suited to clock signals to peripherals that have minimum allowed
 - periods.
 -
 - For more documentation, see `avg`.
 -}
max :: (KnownNat n, KnownNat (n+1))
    => Unsigned (n+1)
    -> (Unsigned n, Unsigned n, State)
    -> (Unsigned (n+1), Bool)

max s (m, d, cmd) = (s', o)
    where
        sinc = s + resize m
        wrap = s >= resize d
        s'   = if cmd == Clear then
                 1
               else if wrap then
                 1
               else if cmd == Stop then
                 s
               else
                 sinc
        o    = wrap

{-
 - Instantiate a clock scaler with fixed parameters through Template Haskell
 -
 - The needed amount of bits in the state of the scaler is automatically
 - determined from the parameters.
 -}

--static :: (KnownNat n, KnownNat (n+1)
--          , Lift (   Unsigned (n+1)
--                  -> (Unsigned n, Unsigned n, State)
--                  -> (Unsigned (n+1), Bool)))
--       => (   Unsigned (n+1)
--           -> (Unsigned n, Unsigned n, State)
--           -> (Unsigned (n+1), Bool))
--       -> (Integer, Integer)
--       -> ExpQ

staticAvg (m, d) = appE
                       (appE [| staticAvg' |] (integerLitToU m))
                       (integerLitToU d)

staticMax (m, d) = appE
                       (appE [| staticMax' |] (integerLitToU m))
                       (integerLitToU d)


{-
 - Through Template Haskell, instantiate a clock scaler implementation `s` that
 - converts clock rate `from` to ticks with a rate of `to`, with static
 - parameters computed at compile time.
 -}

staticAvgRate from to = staticAvg $ rateParams from to

staticMaxRate from to = staticMax $ rateParams from to

staticAvg' :: ( KnownNat (Max m n), KnownNat (Max m n + 1), KnownNat n
              , KnownNat m)
           => Unsigned n
           -> Unsigned m
           -> Unsigned (Max m n + 1)
           -> State
           -> (Unsigned (Max m n + 1), Bool)
staticAvg' m d st cmd = avg st (resize m, resize d, cmd)

staticMax' :: ( KnownNat (Max m n), KnownNat (Max m n + 1), KnownNat n
              , KnownNat m)
           => Unsigned n
           -> Unsigned m
           -> Unsigned (Max m n + 1)
           -> State
           -> (Unsigned (Max m n + 1), Bool)
staticMax' m d st cmd = avg st (resize m, resize d, cmd)
