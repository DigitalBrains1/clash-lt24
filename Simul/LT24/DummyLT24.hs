{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Simul.LT24.DummyLT24 where

import CLaSH.Prelude
import Control.Applicative
import Debug.Trace

import LT24.LT24 (Action(..))

lt24 i@(action, din, ltdin)
    = (ready, dout, lcd_on, csx, resx, dcx, wrx, rdx, ltdout, oe)
    where
        dout = signal (0 :: Unsigned 16)
        lcd_on = signal H
        csx = signal L
        resx = signal H
        dcx = signal H
        wrx = signal H
        rdx = signal H
        ltdout = din
        oe = signal L

        iD = (unpack . register (NOP, 0, 0) . pack) i
        ready = (lt24' <^> (True, 1)) iD

lt24' :: (Bool, Integer)
      -> (Action, Unsigned 16, Unsigned 16)
      -> ((Bool, Integer), Bool)
lt24'   (False, n) (action, din, _) = trace ("Done") ((True, n), True)
lt24' s@(True, n)  (NOP   , din, _) = (s, True)
lt24'   (True, n)  i@(Write , din, _) = lt24'' n i
lt24'   (True, n)  i@(ReadFM, din, _) = lt24'' n i
lt24'   (True, n)  i@(ReadID, din, _) = lt24'' n i
lt24'   (True, n)    (action, din, _) = trace ( (shows action . (',':)
                                              . shows din) "")
                                              ((False, 1  ), False)

lt24'' n  (action, din, _) = trace ( (shows n . (':':) . shows action
                                   . (',':) . shows din) "")
                                   ((False, n+1), False)
