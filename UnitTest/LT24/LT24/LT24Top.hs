module UnitTest.LT24.LT24.LT24Top where

import CLaSH.Prelude
import Control.Applicative

import qualified LT24.LT24 as LT24

-- Use lt24 as a toplevel entity, for simulation

topEntity i = o
    where
        o = ((combineOutput <$>) . pack)
              (ready, dout, lcd_on, csx, resx, dcx, wrx, rdx, ltdout, oe)
        (action, din, ltdin) = (unpack . (splitInput <$>)) i
        (ready, dout, lcd_on, csx, resx, dcx, wrx, rdx, ltdout, oe)
            = LT24.lt24 (action, din, ltdin)

combineOutput (ready, dout, lcd_on, csx, resx, dcx, wrx, rdx, ltdout, oe)
    = (toBV ready <++> toBV dout
       <++> (lcd_on :> csx :> resx :> dcx :> wrx :> rdx :> Nil)
       <++> toBV ltdout) <: oe

splitInput :: Vec 35 Bit
           -> (LT24.Action, Unsigned 16, Unsigned 16)

splitInput i = (action, din, ltdin)
    where
        actionN = fromBV (vtakeI i) :: Unsigned 3
        din = fromBV (vselect d3 d1 d16 i) :: Unsigned 16
        ltdin = fromBV (vdropI i) :: Unsigned 16

        action = case actionN of
                   1 -> LT24.Reset
                   2 -> LT24.Command
                   3 -> LT24.Write
                   4 -> LT24.ReadFM
                   5 -> LT24.ReadID
                   _ -> LT24.NOP
