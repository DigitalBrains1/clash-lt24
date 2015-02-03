{-# LANGUAGE ExistentialQuantification #-}

module Simul.Common
       ( module Simul.Common
       , module CLaSH.Prelude
       ) where

import qualified Text.Show.Pretty as Pr
import CLaSH.Prelude

-- Pretty print argument
pretty :: Show a => a -> IO ()
pretty = putStrLn . Pr.ppShow

data EventList b = Ticks Int | Time Float | forall a. Set (a -> b) a | Infinity

{-
 - Construct a list of inputs from a list of events
 -
 - The function `eventList` is to deal with the input to CLaSH's simulate
 - function on another conceptual level. A list of events coupled with a
 - user-defined function to convert a "Set" event to a new input is used to
 - generate a list of inputs (i.e., a stream).
 -
 - Arguments:
 - tr - The user-defined state transformer
 - f  - Operating frequency (for using "Time" events)
 - s  - The initial state of the inputs
 - es - The event list
 -
 - As an example, let's define the input of the function to simulate as
 - (Bit, Unsigned 8); i.e., it has two inputs: just a wire and an 8-bit port
 - interpreted as an unsigned number. Let's call the inputs a and n
 - respectively. Suppose we define a type as follows:
 -
 - data SI = A Bit | N (Unsigned 8)
 -
 - Now the state transformer is simply:
 -
 - trSI (a, n) (A a') = (a', n )
 - trSI (a, n) (N n') = (a , n')
 -
 - And we could generate an input stream from an event list as follows:
 -
 - eventList trSI 50e6 (L, 0) [ Set A H, Set N 5, Ticks 1, Set A L, Time 52e-9
 -                            , Set N 0, Infinity ]
 -
 - Every Time or Ticks statement advances time. A Time statement advances to
 - the first clock tick /after/ the amount of time specified has past. A Set
 - statement does not advance time, so you can alter multiple inputs in one
 - clock tick.
 -
 - The statement 'Infinity' simply keeps the inputs the same for an infinite
 - amount of time.
 -}

eventList :: Real f => (c -> b -> c) -> f -> c -> [EventList b] -> [c]

eventList tr f s [] = []
eventList tr f s ((Set i v):es) = eventList tr f (tr s (i v)) es
eventList tr f s ((Ticks n):es) = (replicate n s) ++ eventList tr f s es
eventList tr f s ((Time t):es) = (replicate n s) ++ eventList tr f s es
    where
        n = ceiling (t * (fromRational . toRational) f)
eventList tr f s (Infinity:_) = repeat s
