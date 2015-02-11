{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module LT24.Init
       ( initLt24
       , lt24WithInit
       ) where

import CLaSH.Prelude
import Control.Applicative
import Language.Haskell.TH

import LT24.InitSteps
import qualified LT24.LT24 as LT24
import qualified Simul.LT24.DummyLT24 as DummyLT24
import LT24.Commands
import LT24.Palette (pal5bTo6b)
import qualified Toolbox.ClockScale as CS
import Toolbox.Misc
import Toolbox.FClk

{-
 - Initialise the display controller
 -
 - Initialise the LT24 display controller to accept a 16bpp format, and
 - initialise the palette to a direct color mapping, such that the 5-bits
 - brightness values range from black to full brightness.
 -
 - It's a simple state machine that passes a list of commands and data to
 - LT24.lt24, waiting for acceptance of the command before passing on to the
 - next. It is designed to be daisy-chained with the component that handles the
 - display after initialisation. As long as initialisation is running, the
 - daisy-chained component never sees its command being accepted (by ready going
 - to False). After the initialisation is done, initLt24 is transparent, simply
 - passing the commands and data from the daisy-chained component.
 -
 - `(i, si)` is a pair (index, subindex) that indexes the list of commands to
 - send to LT24.lt24.
 -
 - When `i` reaches 13, initialisation is done. The way `i` = 13 is
 - special-cased in the next-state expression means that the state is quiescent
 - once reaching completion. If it were not special-cased, `ph` would follow
 - the actions of the daisy-chained component, creating useless signal changes
 - in the FPGA.
 -
 - `i` = 10 is handled specially because it is a wait period rather than a
 - command, and `i` = 12 is simply waiting for the last command to complete.
 -
 - 'ph' indicates the phase of command acceptance by LT24.lt24, tracking the
 - `ready` signal.
 -
 - `im` and `ni` stand for "i minus" and "next i" respectively.
 -}

initLt24 = initLt24' <^> (fromInteger $ vlength initSteps - 1, 0, L)

initLt24' :: ($(uToFit $ vlength initSteps + 1), IlSIndex, Bit)
          -> (Bool, LT24.Action, Unsigned 16)
          -> ( ($(uToFit $ vlength initSteps + 1), IlSIndex, Bit)
             , (LT24.Action, Unsigned 16, Bool))

initLt24' (i, si, ph) (ready, actionDaisy, lt24dinDaisy)
    = ((i', si', ph'), (action, lt24din, readyDaisy))
    where
        (action, lt24din, readyDaisy)
            | i == (0 - 2) = (actionDaisy, lt24dinDaisy, ready)
            | i == (0 - 1) = (LT24.NOP   , 0           , True )
            | otherwise    = (myAction   , myLt24din   , True )
        (myAction, myLt24din)
            = case step of
               IlL a d       -> (a         , d           )
               IlDoPalette5b -> (LT24.Write, ( resize
                                             . pal5bTo6b
                                             . resize) si)
               IlDoPalette6b -> (LT24.Write, resize si   )
               _             -> (LT24.NOP  , 0           )
        step = initSteps!i

        (i', si', ph')
            | i == (0 - 2)          = (i , si , ph)
            | i == (0 - 1) && ready = (im, si , ph)
            | i == (0 - 1)          = (i , si , ph)
            | otherwise
                = case (step, ph, ready) of
                    (IlWait n, _, _    ) -> (ni, nsi, L )
                    (_       , L, False) -> (ni, nsi, H )
                    (_       , H, True ) -> (i , si , L )
                    _                    -> (i , si , ph)
        im = i - 1
        sip = si + 1

        (ni, nsi) = case (step, si) of
                      (IlL a d      , _ ) -> (im, 0  )
                      (IlDoPalette5b, 31) -> (im, 0  )
                      (IlDoPalette6b, 63) -> (im, 0  )
                      (IlWait n     , _ ) -> if si == n then
                                               (im, 0  )
                                             else
                                               (i , sip)
                      _                   -> (i , sip)

{-
 - Combines initLt24 and LT24.lt24, with as a result a component with the same
 - interface and operation as just LT24.lt24, but running the initialisation on
 - power on and reset.
 -}

lt24WithInit (action_daisy, din_daisy, ltdin)
    = (ready_daisy, dout, lcd_on, csx, resx, dcx, wrx, rdx, ltdout, oe)
    where
    (ready, dout, lcd_on, csx, resx, dcx, wrx, rdx, ltdout, oe)
        = LT24.lt24 (action, din, ltdin)
--        = DummyLT24.lt24 (action, din, ltdin)
    (action, din, ready_daisy) = initLt24 (ready, action_daisy, din_daisy)
