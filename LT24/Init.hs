module LT24.Init where

import CLaSH.Prelude

import qualified LT24.LT24 as LT24
import LT24.Commands
import LT24.Palette (pal5bTo6b)

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
 - send to LT24.lt24. When `i` reaches 11, initialisation is done. The way i is
 - special-cased in the next-state expression means that the state is quiescent
 - once reaching `i` = 11. If it were not special-cased, `ph` would follow the
 - actions of the daisy-chained component, creating useless signal changes in
 - the FPGA.
 -
 - 'ph' indicates the phase of command acceptance by LT24.lt24, tracking the
 - `ready` signal.
 -
 - `ip` and `ni` stand for "i plus" and "next i" respectively.
 -}

initLt24 = initLt24' <^> (0, 0, L)

initLt24' (i, si, ph) (ready, action_daisy, lt24din_daisy)
    = ((i', si', ph'), (action, lt24din, ready_daisy))
    where
        (action, lt24din, ready_daisy)
            = case i of
                11 -> (action_daisy, lt24din_daisy, ready)
                _  -> (my_action   , my_lt24din   , True )
        (my_action, my_lt24din) = initLt24'' (i, si)
        ip = i + 1
        sip = si + 1
        (ni, nsi) = case (i, si) of
                      ( 6, 31) -> (ip, 0  )
                      ( 6, _ ) -> (i , sip)
                      ( 7, 63) -> (ip, 0  )
                      ( 7, _ ) -> ( i, sip)
                      ( 8, 31) -> (ip, 0  )
                      ( 8, _ ) -> (i , sip)
                      ( _, _ ) -> (ip, 0  )
        (i', si', ph') = case (i, ph, ready) of
                           (11, _, _    ) -> (i , si , ph)
                           (_ , L, False) -> (ni, nsi, H )
                           (_ , H, True)  -> (i , si , L )
                           _              -> (i , si , ph)

initLt24'' :: (Unsigned 4, Unsigned 6)
           -> (LT24.Action, Unsigned 16)

initLt24'' ( 0, _) = (LT24.Reset  , 0            )
initLt24'' ( 1, _) = (LT24.Command, cMADCTL      )
initLt24'' ( 2, _) = (LT24.Write  , 8            ) -- BGR subpixel order
initLt24'' ( 3, _) = (LT24.Command, cCOLMOD      )
initLt24'' ( 4, _) = (LT24.Write  , 5            ) -- 16bpp MCU interface
initLt24'' ( 5, _) = (LT24.Command, cRGBSET      )
initLt24'' ( 6, b) = (LT24.Write  , (resize
                                    . pal5bTo6b
                                    . resize)   b) -- Red
initLt24'' ( 7, b) = (LT24.Write  , resize b     ) -- Green
initLt24'' ( 8, b) = (LT24.Write  , (resize
                                    . pal5bTo6b
                                    . resize)   b) -- Blue
initLt24'' ( 9, _) = (LT24.Command, cSLPOUT      )
initLt24'' (10, _) = (LT24.Command, cDISPON      )

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
    (action, din, ready_daisy) = initLt24 (ready, action_daisy, din_daisy)
