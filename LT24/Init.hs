{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-
 - Copyright (c) 2015, Peter Lebbing <peter@digitalbrains.com>
 - All rights reserved.
 - 
 - Redistribution and use in source and binary forms, with or without
 - modification, are permitted provided that the following conditions are met:
 - 
 - 1. Redistributions of source code must retain the above copyright notice,
 - this list of conditions and the following disclaimer.
 - 
 - 2. Redistributions in binary form must reproduce the above copyright notice,
 - this list of conditions and the following disclaimer in the documentation
 - and/or other materials provided with the distribution.
 - 
 - THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 - AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 - IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 - ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 - LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 - CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 - SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 - INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 - CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 - ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 - POSSIBILITY OF SUCH DAMAGE.
 -}

module LT24.Init
       ( initLt24
       , lt24WithInit
       ) where

import CLaSH.Prelude
import Control.Applicative
import Language.Haskell.TH

import LT24.InitSteps
import qualified LT24.LT24 as LT24
import LT24.LT24 (lt24)
--import Simul.LT24.DummyLT24 (lt24)
import LT24.Commands
import LT24.Palette (pal5bTo6b)
import qualified Toolbox.ClockScale as CS
import Toolbox.Misc
import Toolbox.FClk

{-
 - Initialize the display controller
 -
 - Runs through all elements of the initSteps vector. This initializes the
 - display.
 -
 - Every element in initSteps can be one of:
 -
 -      IlL a d        -- Literal: send an action a and din d to lt24.
 -      IlDoPalette5b  -- Write the 32 palette values that initialise the
 -                        palette of a subpixel to a direct colour mapping.
 -      IlDoPalette6b  -- Write the 64 direct colour palette values for green
 -      IlWait n       -- Wait for n clockticks
 -      IlWipeFb       -- Write 320*240 (=76,800) zeroes to the display
 -
 - It's a simple state machine walking the vector, executing each element as
 - indicated above and waiting for acceptance of the command before passing
 - on to the next.
 -
 - It is designed to be daisy-chained with the component that handles the
 - display after initialisation. As long as initialisation is running, the
 - daisy-chained component never sees its command being accepted (by ready
 - going to False). After the initialization is done, initLt24 is transparent,
 - simply passing the commands and data from the daisy-chained component.
 -
 - `i` indexes initSteps. After running from the highest-numbered, i.e.,
 - first, element down to 0, there are two further final steps which are
 - numbered -1 and -2. By writing it as (0 - 1) we can still work with an
 - Unsigned for i. Step -1 waits for LT24.lt24 to complete, and -2 is the
 - final state, which is never left, where initLt24 is transparent and
 - dormant.
 -
 - `i` needs to be large enough to address all initSteps elements, in other
 - words, (vlength initSteps - 1), plus the two final states -1 and -2. Hence
 - the uToFit expression in the type declaration below.
 -
 - `si` is "subindex" and keeps track of the progress of the execution of the
 - current element from initSteps. For instance, it iterates the palette
 - values, and counts ticks in IlWait.
 -
 - `ph` indicates the phase of command acceptance by LT24.lt24, tracking the
 - `ready` signal.
 -
 - Interpretation hint: `im` and `ni` stand for "i minus" and "next i"
 - respectively.
 -
 -}

initLt24 = initLt24' <^> (fromInteger $ vlength initSteps - 1, 0, L)

initLt24' :: ($(uToFit $ vlength initSteps + 1), IlSIndex, Bit)
          -> (Bool, LT24.Action, Unsigned 16)
          -> ( ($(uToFit $ vlength initSteps + 1), IlSIndex, Bit)
             , (LT24.Action, Unsigned 16, Bool))

initLt24' (i, si, ph) (ready, actionDaisy, dInDaisy)
    = ((i', si', ph'), (action, din, readyDaisy))
    where
        -- Daisy chain handling
        (action, din, readyDaisy)
            | i == (0 - 2) = (actionDaisy, dInDaisy, ready)
            | i == (0 - 1) = (LT24.NOP   , 0       , True )
            | otherwise    = (myAction   , myDIn   , True )

        (myAction, myDIn)
            = case step of
               IlL a d       -> (a         , d           )
               IlDoPalette5b -> (LT24.Write, ( resize
                                             . pal5bTo6b
                                             . resize) si)
               IlDoPalette6b -> (LT24.Write, resize si   )
               IlWipeFb      -> (LT24.Write, 0           )
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
                      (IlL a d      , _    ) -> (im, 0  )
                      (IlDoPalette5b, 31   ) -> (im, 0  )
                      (IlDoPalette6b, 63   ) -> (im, 0  )
                      (IlWait n     , _    ) -> if si == n then
                                                  (im, 0  )
                                                else
                                                  (i , sip)
                      (IlWipeFb     , 76799) -> (im, 0  ) -- 320 * 240 - 1
                      _                      -> (i , sip)

{-
 - Combines initLt24 and LT24.lt24, with as a result a component with the same
 - interface and operation as just LT24.lt24, but running the initialisation on
 - power on and reset.
 -}

lt24WithInit (actionDaisy, dInDaisy, ltdin)
    = (readyDaisy, dout, lcdOn, csx, resx, dcx, wrx, rdx, ltdout, oe)
    where
    (ready, dout, lcdOn, csx, resx, dcx, wrx, rdx, ltdout, oe)
        = lt24 (action, din, ltdin)
    (action, din, readyDaisy) = initLt24 (ready, actionDaisy, dInDaisy)
