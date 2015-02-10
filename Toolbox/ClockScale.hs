{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Toolbox.ClockScale
       ( rateParams
       , State(..)
       , avg
       , clkDiv
       , oneShot
       , staticAvg
       , staticClkDiv
       , staticOneShot
       , staticAvgRate
       , staticMaxRate
       , staticOneShotPeriod
       , ticksMinPeriod
       , ticksMinPeriodTH
       , ticksMaxRate
       , ticksMaxRateTH
       , nonNeg
       ) where

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
 - this is the convenient "staticAvgRate" function in this module that can be
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

{- Divide the clock by an integer ratio
 -
 - See `avg` for more documentation.
 -}

clkDiv :: KnownNat n
       => Unsigned n
       -> (Unsigned n, State)
       -> (Unsigned n, Bool)

clkDiv s (d, cmd) = (s', o)
    where
        wrap = s >= d
        s'   = if cmd == Clear then
                 1
               else if wrap then
                 1
               else if cmd == Stop then
                 s
               else
                 s + 1
        o    = wrap

{-
 - When triggered, count to d once, then stop
 -
 - Outputs whether d has been reached yet. Most often used to make sure an
 - amount of work does not take less time than counting to d: trigger when
 - you start the work, and then wait for the oneShot to output True when the
 - work is done. If the amount of work took more than the allotted time,
 - this makes it continue immediately; otherwise it will wait.
 -}
oneShot :: KnownNat n
        => Unsigned n
        -> (Unsigned n, Bool)
        -> (Unsigned n, Bool)

oneShot s (d, trigger) = (s', done)
    where
        done = s >= d
        s' = if trigger then
               1
             else if done then
               d
             else
               s + 1

{-
 - Instantiate a clock scaler with fixed parameters through Template Haskell
 -
 - The needed amount of bits in the state of the scaler is automatically
 - determined from the parameters.
 -}

staticAvg (m, d) = appE (appE [| staticAvg' |] (fitU m)) (fitU d)
staticClkDiv d = appE [| staticClkDiv' |] (fitU d)

{-
 - Instantiate a oneShot with a fixed divider through Template Haskell
 -}

staticOneShot d = appE [| staticOneShot' |] (fitU d)

{-
 - Through Template Haskell, instantiate a clock scaler that converts clock
 - rate `from` to ticks with a rate of `to`, with static parameters computed at
 - compile time.
 -}

staticAvgRate from to = staticAvg $ rateParams from to

{-
 - Through Template Haskell, instantiate a clock divider that never exceeds the
 - rate `to`.
 -
 - `staticAvgRate` can temporarily exceed the given rate (by one clock tick) to
 - compensate for time lost. `staticMaxRate` however will never do that, but
 - this means it will run slow if the target rate does not evenly divide the
 - clock frequency.
 -}

staticMaxRate from to = staticClkDiv $ ticksMaxRate from to

{-
 - Instantiate a oneShot that waits for the specified period through
 - Template Haskell
 -
 - `f` is the clock frequency, `p` the period.
 -}

staticOneShotPeriod f p = staticOneShot $ ticksMinPeriod f p

{-
 - Compute how many clock ticks need to pass before a certain period of time
 - is reached
 -
 - `f` is the clock frequency, `p` the period.
 -}

ticksMinPeriod f p = ceiling $ (fromInteger f) * p - 1

-- Shorthand for Template Haskell instantiation

ticksMinPeriodTH f p = intLit $ ticksMinPeriod f p

{-
 - Compute which clock divider approximates the given rate as closely as
 - possible without exceeding it
 -}

ticksMaxRate from to = ticksMinPeriod from (1 / to)

-- Shorthand for Template Haskell instantiation

ticksMaxRateTH from to = intLit $ ticksMaxRate from to

{- 
 - When computing a delay in clock ticks, this function assures that when the
 - result of the computation is negative, no delay is done (a delay of 0).
 -
 - Example: $(nonNeg $(ticksMinPeriod fClk 100e-9) - 4)
 -   Computes the extra delay to have a total delay of 100 ns of which 4 clock
 -   ticks have already been spent. If 4 clock ticks are more than 100 ns,
 -   don't delay.
 -}

nonNeg = intLit . max 0


staticAvg' :: ( KnownNat (Max m n), KnownNat (Max m n + 1), KnownNat n
              , KnownNat m)
           => Unsigned n
           -> Unsigned m
           -> Unsigned (Max m n + 1)
           -> State
           -> (Unsigned (Max m n + 1), Bool)

staticAvg' m d st cmd = avg st (resize m, resize d, cmd)

staticClkDiv' d s cmd = clkDiv s (d, cmd)

staticOneShot' d s trigger = oneShot s (d, trigger)
