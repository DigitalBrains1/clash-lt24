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
        ready = (lt24' <^> True) iD

lt24' :: Bool
      -> (Action, Unsigned 16, Unsigned 16)
      -> (Bool, Bool)
lt24' False (action, din, _) = (True, True)
lt24' True  (NOP   , din, _) = (True, True)
lt24' True  (action, din, _) = trace ((shows action . (',':) . shows din) "")
                                  (False, False)
